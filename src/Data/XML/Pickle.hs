{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
            UndecidableInstances, FunctionalDependencies, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides XML picklers that plug into the xml tree of the
-- /xml-types/ package.
-- This module was \"inspired\" by hexpat-pickle.
--
-- The API differences between /hexpat-pickle/ and this module include:
--
--  * When unpickling, picklers will /consume/ matching elmements so that they will be ignored by sucessive picklers.
--  To circumvent this behaviour, use @'xpPeek'@
--
--  * wrappers like 'xpWrap' are uncurried
--
--  * There are no lazy unpicklers
--
--  * Unpicklers keep track of whether they (and any nested picklers) consumed all input, giving rise to the 'xpClean' and 'xpRecursiveClean' combinators
--
-- The data type @'PU' t a@ represents both a pickler (converting Haskell data
-- to XML) and an unpickler (XML to Haskell data), so your code only needs to be
-- written once for both serialization and deserialization.  The 'PU' primitives, such
-- as 'xpElem' for XML elements, may be composed into complex arrangements using
-- 'xpPair' and other combinators.
--
-- The reason why you a list of nodes instead of just one when working with a single
-- element is because the unpickler of 'xpElem' needs to see the whole list of nodes
-- so that it can 1. skip whitespace, and 2. search to match the specified tag name.
--
-- Most picklers will try to find the /first match/ rather than failing when
-- the first element doesn't match. This is why the target type often ist
-- a list. To prevent this behaviour and commit the pickler to the first
-- element available, use /xpIsolate/.
--
-- The top level of the document does not follow this rule, because it is a single
-- node type.  'xpRoot' is needed to adapt this to type ['Node'] for your
-- pickler to use.  You would typically define a pickler for a whole document with
-- 'xpElem', then pickle it to a single 'Node' with @'pickleTree' (xpRoot myDocPickler) value@.
--
-- /NB/: Unresolved entities are considered an error and will trigger an exception
--
-- When unpickling, the folowing invariant regarding the list of remaining elements should be observed:
-- * The returned list should be a subset of or the initial list itself, that is, no elements should be added
-- or changed
-- * The relative order of elements should be preserved
-- * Elements may, however, be removed from anywhere in the list
--
-- Here is a simple example to get you started:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Text
-- > import Data.XML.Types
-- > import Data.XML.Pickle
-- >
-- > -- Person name, age and description
-- > data Person = Person Text Int Text
-- >
-- > xpPerson :: PU [Node] Person
-- > xpPerson =
-- >     -- How to wrap and unwrap a Person
-- >     xpWrap (\((name, age), descr) -> Person name age descr)
-- >            (\(Person name age descr) -> ((name, age), descr)) $
-- >     xpElem "person"
-- >         (xpPair
-- >             (xpAttr "name" xpId)
-- >             (xpAttr "age" xpPrim))
-- >         (xpContent xpId)
-- >
-- > people = [
-- >     Person "Dave" 27 "A fat thin man with long short hair",
-- >     Person "Jane" 21 "Lives in a white house with green windows"]
-- >
-- > main = do
-- >     print $ pickle (xpRoot $ xpElemNodes "people" $ xpAll xpPerson) people
--
-- Program outputs would be an xml-value equivalent to:
--
-- > <people><person name="Dave" age="27">A fat thin man with long short hair</person>
-- > <person name="Jane" age="21">Lives in a white house with green windows</person></people>
--
-- Funktions marked with /compat/ are included for compatibility with hexpat-pickle

module Data.XML.Pickle (
  -- * Types
  PU(..)
  , Attribute
  -- * Pickler Invocation
  , pickle
  , unpickle
   -- * Primitive picklers
  , xpUnit
  , xpZero
  , xpThrow
   -- * Value-preserving picklers
  , xpId
  , xpTrees
  , xpHead
  , xpTree
  , xpText0
  , xpRoot
  , xpPrim
  -- * XML specific picklers
  -- ** Attributes
  , xpAttr
  , xpAttrImplied
  , xpAttrFixed
   -- ** Elements
  , xpElem
  , xpElemWithName
  , xpElemByNamespace
  , xpElemVerbatim
  , xpElemAttrs
  , xpElemNodes
  , xpElemBlank
  , xpElemExists
   -- ** Character Content
  , xpContent
   -- * Pickler combinators
   -- ** choice
  , xpOption
  , xpDefault
  , xpWithDefault
  , xpMap
  , xpAlt
  , xpEither
  , xpTryCatch
  -- ** sequencing
  -- |
  -- /NB/ The sequencing operations /do not/ enforce any order on the
  -- matched elements unless stated otherwise, but you can commit individial
  -- picklers to the next available element with 'xpIsolate'.
  -- Applying @xpIsolate@ on all nested Picklers will in effect enforce order.
  --
  -- Howver, once a pickler consumes an element it will not be available to
  -- following picklers. You can circumvent this behaviour with 'xpPeek'.
  --
  -- If you want ensure that all elements are consumed after the last pickler is
  -- run you may want to use 'xpClean'

  -- *** Lists
  -- |
  -- The List pickler combinators will pickle lists in the given order
  -- without any special treatment and unpickle as stated.
  , xpFindMatches
  , xpAll
  , xpList0
  , xpSeqWhile
  , xpList
  -- *** Tuples
  -- | Tuple combinators apply their picklers from left to right
  , xp2Tuple
  , xpPair
  , (<++>)
  , xp3Tuple
  , xpTriple
  , xp4Tuple
  , xp5Tuple
  , xp6Tuple
  -- ** Wrappers
  -- *** value wrappers
  , xpWrap
  , xpWrapEither
  , xpWrapMaybe
  , xpWrapMaybe_
  -- *** Book keeping
  -- | Change the semantics of picklers
  , xpIsolate
  , xpPeek
  -- *** Cleannes
  -- |
  -- Picklers keep track of elements left over after unpickling,
  -- so the may be
  --
  -- [@clean@] an unpickling is considered @clean@ when it doesn't leave any remainng elements
  --
  -- [@recursively clean@] an unpickling is considered @recursively clean@ if it and any nested picklers are clean
  --

  , xpClean
  , xpRecursiveClean
  -- * Exceptions
  , UnresolvedEntityException
  -- * helper functions
  , flattenContent
) where

import Control.Applicative ((<$>))
import Control.Arrow

import Data.Either
import Data.List(intersperse)
import Data.Monoid(Monoid, mempty)

import Control.Exception
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Data.XML.Types

data PU t a = PU
  { unpickleTree :: t
                    -> Either String (a, (Maybe t, Bool))
                    -- ^ Either an error or the return value,
                    -- any remaining input and a Bool value indicating whether
                    -- all nested picklers where clean
  , pickleTree :: a -> t
  }

data UnresolvedEntityException = UnresolvedEntityException
                                   deriving (Typeable, Show)
instance Exception UnresolvedEntityException

-- | pickle a Tree
pickle :: PU t a -> a -> t
pickle = pickleTree

-- | unpickle a tree, throws away information concerning cleannes
unpickle :: PU t a -> t -> Either String a
unpickle xp x = fst <$> unpickleTree xp x


for :: [a] -> (a -> b) -> [b]
for = flip map

mapLeft _ (Right r) = Right r
mapLeft f (Left l ) = Left $ f l

type Attribute = (Name,[Content])

-- | Returns everything (remaining), untouched.
xpId :: PU a a
xpId = PU (\t -> Right (t, (Nothing,True))) id

-- | 'xpId' (/compat/)
xpTrees :: PU a a
xpTrees = xpId

-- | return one element, untouched
xpHead :: PU [a] a
xpHead = PU {unpickleTree = \t -> case t of
                [] -> Left $ "xpHead: No element remaining"
                t:ts -> Right (t , (if null ts then Nothing else Just ts, True))
            , pickleTree = return
            }

-- | 'xpHead' (/compat/)
xpTree :: PU [a] a
xpTree = xpHead

-- | specialised version of 'xpId' (/compat/)
xpText0 :: PU Text Text
xpText0 = xpId


-- | Adapts a list of nodes to a single node. Generally used at the top level of
-- an XML document.
xpRoot ::PU [a] b -> PU a b
xpRoot pa = PU
       { unpickleTree = \t -> case unpickleTree pa [t] of
            Left l -> Left l
            Right (a,(Nothing,True)) -> Right (a, (Nothing,True ))
            Right (a,(_      ,_   )) -> Right (a, (Nothing,False))
       , pickleTree = \t -> case pickleTree pa t of
           [t1] -> t1
           _    -> error "pickler called by xpRoot must output only one element"
       }


getFirst :: (t -> Bool) -> [t] -> Maybe (t, [t])
getFirst _ [] = Nothing
getFirst p (x:xs) = case p x of
      True  -> Just (x,xs)
      False -> (second (x:)) <$> getFirst p xs


-- | pickle to/from attribute
xpAttr :: Name -> PU Text a -> PU [Attribute] a
xpAttr name pu = PU
        { unpickleTree = doUnpickle
        , pickleTree = \value -> [(name, [ContentText $ pickleTree pu value])]
        }
  where
    doUnpickle attrs = case getFirst ((== name) . fst) attrs of
      Nothing -> Left $ "attribute " ++ show name ++ " not found."
      Just ((_,[ContentText x]), rem) -> case unpickleTree pu x of
        Left e -> Left $ "in attribute " ++ show name ++ " : " ++ e
        Right (y,(_,c)) -> let rem' = if null rem then Nothing else Just rem
                         in Right (y,(rem',c))
      _ -> Left $ "xpAttr: Unresolved entities in " ++ show name ++ "."

-- | Pickle attribute if Just is given, on unpickling return Just <val> when
-- the attribute is found, Nothing otherwise
xpAttrImplied :: Name -> PU Text a -> PU [Attribute] (Maybe a)
xpAttrImplied name pu = xpOption $ xpAttr name pu

-- | Pickle an attribute with the specified name and value, fail if the same attribute is
-- not present on unpickle.
xpAttrFixed :: Name -> Text -> PU [Attribute] ()
xpAttrFixed name val =
    xpWrapMaybe_ ("expected fixed attribute "++ show name++"="++show val)
                (\v -> if v == val then Just () else Nothing) (const val) $
    xpAttr name xpId

-- | Add an attribute with a fixed value.
xpAddFixedAttr :: Name -> Text -> PU [Attribute] b -> PU [Attribute] b
xpAddFixedAttr name val pa
    = xpWrap snd ((,) ()) $
      xp2Tuple (xpAttrFixed name val) pa


-- merge successive NodeCotents
flattenContent :: [Node] -> [Node]
flattenContent xs = case foldr (\x (buf, res) -> case x of
                         NodeContent (ContentText txt)
                           -> (txt : buf, res)
                         e@(NodeElement _)
                           -> ([] , e : addConcatText buf res)
                         _ -> throw UnresolvedEntityException
                    ) ([], []) xs
                   of
                   (buf, res) -> addConcatText buf res
  where
    nc = NodeContent . ContentText
    addConcatText [] = id
    addConcatText xs = (nc (Text.concat xs) :)


-- | When unpickling, tries to find the first element with the supplied name.
-- Once such an element is found, it will commit to it and /fail/ if any of the
-- picklers don't match.
xpElem :: Name -- ^ name of the Element
          -> PU [Attribute] a -- ^ pickler for attributes
          -> PU [Node] n  -- ^ pickler for child nodes
          -> PU [Node] (a,n)
xpElem name attrP nodeP = PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \(a,n) -> [NodeElement $ Element name
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst (nodeElementNameHelper name) nodes of
      Just ((NodeElement (Element _ attrs children)), rem) ->
        case unpickleTree attrP attrs of
          Left e -> Left $ "in element " ++ show name ++ " : " ++ e
          Right (x,(_,ca)) -> case unpickleTree nodeP
                                    $ flattenContent children of
            Left e -> Left $ "in element " ++ show name ++ " : " ++ e
            Right (y,(_,cc)) ->
              let rem' = if null rem then Nothing else Just rem
              in Right ((x,y), (rem' , ca && cc))
      _ -> Left $ "xpElem: " ++ show name ++ " not found."

    nodeElementNameHelper name (NodeElement (Element n _ _)) = n == name
    nodeElementNameHelper _ _ = False

-- | pickle Element without restriction on the name.
-- the name as taken / returned as the first element of the triple
xpElemWithName :: PU [Attribute] a  -- ^ pickler for attributes
                  -> PU [Node] n    -- ^ pickler for child nodes
                  -> PU [Node] (Name,a,n)
xpElemWithName attrP nodeP = PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \(name, a,n) -> [NodeElement $ Element name
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst nodeElementHelper nodes of
      Just ((NodeElement (Element name attrs children)), rem) ->
        case unpickleTree attrP attrs of
          Left e -> Left $ "in element " ++ show name ++ " : " ++ e
          Right (x,(_,ca)) -> case unpickleTree nodeP $
                                     flattenContent children of
            Left e -> Left $ "in element " ++ show name ++ " : " ++ e
            Right (y,(_,cc)) ->
              let rem' = if null rem then Nothing else Just rem
              in Right ((name,x,y), (rem' , ca && cc))
      _ -> Left $ "xpElemWithName: no element found."

    nodeElementHelper (NodeElement (Element _ _ _)) = True
    nodeElementHelper _ = False

-- | find element by name space, prefixes are ignored
xpElemByNamespace :: Text -- ^ Namespace
                  -> PU Text name -- ^ Pickler for the local name
                  -> PU [Attribute] a  -- ^ pickler for attributes
                  -> PU [Node] n    -- ^ pickler for child nodes
                  -> PU [Node] (name,a,n)
xpElemByNamespace ns nameP attrP nodeP = PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \(name, a,n) -> [NodeElement $ Element
                                     (Name (pickleTree nameP name) (Just ns) Nothing)
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst (nodeElementNSHelper ns) nodes of
      Just ((NodeElement (Element name attrs children)), rem) ->
        case (do
          (name' , (_, cn)) <- unpickleTree nameP (nameLocalName name)
          (attrs', (_, ca)) <- unpickleTree attrP attrs
          (nodes', (_, cns)) <- unpickleTree nodeP nodes
          return ((name', attrs', nodes')
                 ,(if null rem then Nothing else Just rem,   cn && ca && cns))
        ) of
          Left e -> Left $ "in xpElemByNamespace with element " ++ show name ++ " : " ++ e
          Right r -> Right r
      _ -> Left $ "xpElemWithName: no element found."

    nodeElementNSHelper ns (NodeElement (Element n _ _)) = nameLocalName n == ns
    nodeElementNSHelper ns _ = False

-- | use Element untouched
xpElemVerbatim ::  PU [Node] (Element)
xpElemVerbatim = PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \e -> [NodeElement e]
         } where
    doUnpickleTree nodes = case getFirst nodeElementHelper nodes of
      Just ((NodeElement e@(Element _ _ _)), rem) ->
        let rem' = if null rem then Nothing else Just rem
        in Right (e, (rem', True))
      _ -> Left $ "xpElemVerbatim: no element found."

    nodeElementHelper (NodeElement (Element _ _ _)) = True
    nodeElementHelper _ = False

-- | A helper variant of xpElem for elements that contain attributes but no child tags.
xpElemAttrs :: Name -> PU [Attribute] b -> PU [Node] b
xpElemAttrs name puAttrs = xpWrap (fst) (\a -> (a,())) $
                             xpElem name puAttrs xpUnit

-- | A helper variant of xpElem for elements that contain child nodes but no attributes.
xpElemNodes :: Name -> PU [Node] b -> PU [Node] b
xpElemNodes name puChildren = xpWrap (snd) (\a -> ((),a)) $
                                xpElem name xpUnit puChildren

-- | Helper for Elements that don't contain anything
xpElemBlank :: Name -> PU [Node] ()
xpElemBlank name = xpWrap (const () ) (const ((),())) $
                                xpElem name xpUnit xpUnit

-- | When pickling, creates an empty element iff parameter is True
--
-- When unpickling, checks whether element exists
xpElemExists :: Name -> PU [Node] Bool
xpElemExists name = xpWrap (\x -> case x of Nothing -> False; Just _ -> True)
                           (\x -> if x then Just () else Nothing) $
                           xpOption (xpElemBlank name)


-- | Get the Content from a node
xpContent :: PU Text a -> PU [Node] a
xpContent xp = PU
       { unpickleTree = doUnpickle
       , pickleTree = return . NodeContent . ContentText . pickleTree xp
       } where
     doUnpickle nodes = case getFirst nodeContentHelper nodes of
       Just ((NodeContent (ContentText t)), rem) -> case unpickleTree xp t of
         Right (a,(_,c)) -> Right (a, (if null rem then Nothing else Just rem,c))
         Left l -> Left $ "In xpContent: " ++ l
       Just ((NodeContent (ContentEntity t)), _) ->
           Left $ "xpContent: unresolved entity" ++ show t ++ "."
       _ -> Left $ "xpContent: No content found"

     nodeContentHelper (NodeContent _) = True
     nodeContentHelper _ = False

-- | Convert XML text \<-\> a Maybe type. During unpickling, Nothing is returned
-- if there's a failure during the unpickling of the first argument.  A typical
-- example is:
--
-- > xpElemAttrs "score" $ xpOption $ xpAttr "value" xpickle
--
-- in which @Just 5@ would be encoded as @\<score value=\"5\"\/\>@ and @Nothing@ would be
-- encoded as @\<score\/\>@.
xpOption :: PU [t] a -> PU [t] (Maybe a)
xpOption pu = PU {
        unpickleTree = Right . doUnpickle,
        pickleTree = \mValue ->
            case mValue of
                Just value -> pickleTree pu value
                Nothing    -> []
    }
  where
    doUnpickle t =
        case unpickleTree pu t of
            Right (val, (rest, clean)) -> ((Just val), (rest, clean))
            Left  _    -> (Nothing, (Just t, True))

-- | Optional conversion with default value
--
-- Unlike 'xpWithDefault' the default value is not encoded in the XML document,
-- during unpickling the default value is inserted if the pickler fails
xpDefault :: (Eq a) => a -> PU [t] a -> PU [t] a
xpDefault df
    = xpWrap (fromMaybe df)
             (\ x -> if x == df then Nothing else Just x)
             .
      xpOption

-- | Attempt to use a pickler. On failure, return a default value.
--
-- Unlike 'xpDefault', the default value /is/ encoded in the XML document.
xpWithDefault :: a -> PU t a -> PU t a
xpWithDefault a pa = xpTryCatch pa (lift a)
  where
    -- use this instead of standard xpLift, allowing us to use a more general tree type
    lift a = PU
      { unpickleTree = \t -> Right (a, (Just t, True))
      , pickleTree = error "xpWithDefault impossible" -- xpTryCatch never runs the second pickler
      }

-- TODO:
-- We could use Monoid m => m instead of [a], but that is for another day...

-- | Try to extract the reaming elements, fail if there are none
getRest :: (a, (Maybe [r], c)) -> (a, ([r], c))
-- getRest (_, (Nothing, _)) = Left $ "Not enough elements"
getRest (l, (Just r, c)) = (l,(r, c))
getRest (l, (Nothing, c)) =(l,([], c))


-- | Doesn't create or consume anything, always succeeds
xpUnit :: PU [a] ()
xpUnit = PU (\x -> Right ((), (Just x, True))) (const [])


-- | Combines 2 picklers

xp2Tuple :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xp2Tuple xp1 xp2 = PU {pickleTree = \(t1, t2) ->
                        pickleTree xp1 t1 ++ pickleTree xp2 t2
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp2Tuple: " ++) $ do
    -- The /Either String/ monad
    (x1 ,(r1,c1)) <- getRest <$> unpickleTree xp1 r0
    (x2 ,(r ,c2)) <-             unpickleTree xp2 r1
    return ((x1,x2),(r,c1 && c2))

-- | 'xp2Tuple' (/compat/)
xpPair :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xpPair = xp2Tuple

-- | 'xp2Tuple'
(<++>) :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
(<++>) = xp2Tuple

-- | Combines 3 picklers
xp3Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xp3Tuple xp1 xp2 xp3 = PU {pickleTree = \(t1, t2, t3) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp3Tuple: " ++) $ do
    (x1 ,(r1,c1)) <- getRest <$> unpickleTree xp1 r0
    (x2 ,(r2,c2)) <- getRest <$> unpickleTree xp2 r1
    (x3 ,(r ,c3)) <-             unpickleTree xp3 r2
    return ((x1,x2,x3),(r, c1 && c2 && c3))

-- | 'xp3Tuple' (/compat/)
xpTriple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xpTriple = xp3Tuple

-- | Combines 4 picklers
xp4Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4
             -> PU [a] (a1, a2, a3,a4)
xp4Tuple xp1 xp2 xp3 xp4
     = PU {pickleTree = \(t1, t2, t3, t4) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp4Tuple: " ++) $ do
    (x1 ,(r1, c1)) <- getRest <$> unpickleTree xp1 r0
    (x2 ,(r2, c2)) <- getRest <$> unpickleTree xp2 r1
    (x3 ,(r3, c3)) <- getRest <$> unpickleTree xp3 r2
    (x4 ,(r , c4)) <-             unpickleTree xp4 r3
    return ((x1,x2,x3,x4),(r, c1 && c2 && c3 && c4))

-- | Combines 5 picklers
xp5Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4 -> PU [a] a5
             -> PU [a] (a1, a2, a3, a4, a5)
xp5Tuple xp1 xp2 xp3 xp4 xp5
  = PU {pickleTree = \(t1, t2, t3, t4, t5) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                        ++ pickleTree xp5 t5
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp5Tuple: " ++) $ do
    (x1 ,(r1,c1)) <- getRest <$> unpickleTree xp1 r0
    (x2 ,(r2,c2)) <- getRest <$> unpickleTree xp2 r1
    (x3 ,(r3,c3)) <- getRest <$> unpickleTree xp3 r2
    (x4 ,(r4,c4)) <- getRest <$> unpickleTree xp4 r3
    (x5 ,(r ,c5))  <-             unpickleTree xp5 r4
    return ((x1,x2,x3,x4,x5),(r, c1 && c2 && c3 && c4 && c5))

-- | You guessed it ... Combines 6 picklers
xp6Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4 -> PU [a] a5
             -> PU [a] a6
             -> PU [a] (a1, a2, a3, a4, a5, a6)
xp6Tuple xp1 xp2 xp3 xp4 xp5 xp6
  = PU {pickleTree = \(t1, t2, t3, t4, t5, t6) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                        ++ pickleTree xp5 t5
                        ++ pickleTree xp6 t6
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp5Tuple: " ++) $ do
    (x1 ,(r1,c1)) <- getRest <$> unpickleTree xp1 r0
    (x2 ,(r2,c2)) <- getRest <$> unpickleTree xp2 r1
    (x3 ,(r3,c3)) <- getRest <$> unpickleTree xp3 r2
    (x4 ,(r4,c4)) <- getRest <$> unpickleTree xp4 r3
    (x5 ,(r5,c5)) <- getRest <$> unpickleTree xp5 r4
    (x6 ,(r ,c6))  <- {- return-} unpickleTree xp6 r5
    return ((x1,x2,x3,x4,x5,x6),(r, c1 && c2 && c3 && c4 && c5 && c6))

-- | When unpickling, don't consume the matched element(s), noop when pickling
xpPeek :: PU t a -> PU t a
xpPeek xp = PU { pickleTree = pickleTree xp
               , unpickleTree = \xs ->
                  case unpickleTree xp xs of
                    Left e -> Left e
                    Right (r,(_,c)) -> Right (r,(Just xs,c))
               }

-- | Noop when pickling
--
-- When unpickling, only give access to the first element
xpIsolate :: PU [t] a -> PU [t] a
xpIsolate xp = PU { pickleTree = pickleTree xp
               , unpickleTree = \xs -> case xs of
                 [] -> Left $ "xpIsolate: no elements left"
                 (x:xs) -> case unpickleTree xp [x] of
                   Left l -> Left l
                   Right (v,(r,c)) -> Right (v,(handleRest r xs, c))
               } where
  handleRest r xs = case mbToList r ++ xs of [] -> Nothing; rs -> Just rs
  mbToList Nothing = []
  mbToList (Just r) = r

-- | apply a bijection before pickling / after unpickling
xpWrap :: (a -> b) -> (b -> a) -> PU t a -> PU t b
xpWrap to from xp = PU { unpickleTree = \x -> (first to) <$> unpickleTree xp x
                       , pickleTree = pickleTree xp . from
                       }

-- | Convert XML text content \<-\> any type that implements 'Read' and 'Show'.
-- Fails on unpickle if 'read' fails.
xpPrim :: (Show a, Read a) => PU Text a
xpPrim = PU { unpickleTree = \x -> case reads $ Text.unpack x of
                 []       -> Left $ "In xpPrim: couldn't read " ++ show x ++ "."
                 (r,rest):_  -> Right (r,(Nothing, True))
            ,  pickleTree = Text.pack . show
            }

-- | When unpickling, tries to apply the pickler to all elements
-- returning and consuming only matched elements
xpFindMatches :: PU [b] a -> PU [b] [a]
xpFindMatches xp = PU { unpickleTree = doUnpickleTree
                     , pickleTree = \xs -> pickleTree xp =<< xs
                     } where
  doUnpickleTree xs =
    let (ls, rs) = partitionEithers . for xs $ \x ->
          case unpickleTree xp [x] of
            Left _ -> Left x
            Right (r,(_,c)) -> Right (r, c)
        in Right (fst <$> rs, (if null ls then Nothing else Just ls,all snd rs))

-- | Tries to apply the pickler to all the remaining elements;
-- fails if any of them don't match
xpAll :: PU [a] b -> PU [a] [b]
xpAll xp = PU { unpickleTree = \x ->
                 let Right (xs, (rs,c)) = unpickleTree (xpFindMatches xp) x
                   in case rs of
                      Just (_:_) -> Left "In xpAll: not all Elements matched"
                      _ -> Right (xs, (Nothing,c))
              , pickleTree = \xs -> pickleTree xp =<< xs
              }

-- | 'xpAll' (/compat/)
xpList0 :: PU [a] b -> PU [a] [b]
xpList0 = xpAll

-- | Like xpList, but only succeed during deserialization if at least a
-- minimum number of elements are unpickled.
xpListMinLen :: Int -> PU [a] b -> PU [a] [b]
xpListMinLen ml = xpWrapEither testLength id . xpList
  where
    testLength as
      | length as < ml = Left $ "Expecting at least " ++ show ml ++ " elements"
    testLength as = Right as


-- | When unpickling, sucessively applies pickler to single elements until it
-- fails; returns all matched elements.
xpSeqWhile :: PU [a] b -> PU [a] [b]
xpSeqWhile pu = PU {
        unpickleTree = Right . doUnpickle,
        pickleTree = \t -> (pickleTree pu) =<< t
    }
  where
    doUnpickle [] = ([],(Nothing,True))
    doUnpickle es@(elt:rem) =
                case unpickleTree pu [elt] of
                    Right (val,(_,c)) -> let (xs,(r,c')) = doUnpickle rem in
                                             (val:xs,(r,c && c'))
                    Left _    -> ([],(Just es,True))

-- | 'xpSeqWhile' (/compat/)
xpList :: PU [a] b -> PU [a] [b]
xpList = xpSeqWhile

-- | Standard pickler for maps
--
-- This pickler converts a map into a list of pairs of the form
--
-- > <elt attr="key">value</elt>
xpMap :: Ord k =>
     Name  -- ^ Element name (elt)
     -> Name  -- ^ Attribute name (attr)
     -> PU Text k -- ^ Pickler for keys (key)
     -> PU [Node] a  -- ^ Pickler for values (value)
     -> PU [Node] (M.Map k a)
xpMap en an xpk xpv
    = xpWrap M.fromList
             M.toList
              $
      xpAll $
      xpElem en
          (xpAttr an xpk)
          xpv

-- | Like xpWrap, but strips Just (and treats Nothing as a failure) during unpickling.
xpWrapMaybe :: (a -> Maybe b) -> (b -> a) -> PU t a -> PU t b
xpWrapMaybe = xpWrapMaybe_ "xpWrapMaybe can't encode Nothing value"

-- | Like xpWrap, but strips Just (and treats Nothing as a failure) during unpickling,
-- with specified error message for Nothing value.
xpWrapMaybe_ :: String -> (a -> Maybe b) -> ( b -> a) -> PU t a -> PU t b
xpWrapMaybe_ errorMsg a2b b2a pua = PU {
        unpickleTree = \t -> case unpickleTree pua t of
            Right (val, rest) ->
                case a2b val of
                    Just val' -> Right (val', rest)
                    Nothing   -> Left errorMsg
            Left err  -> Left err,
        pickleTree = \value -> pickleTree pua (b2a value)
    }

-- | Like xpWrap, except it strips Right (and treats Left as a failure) during unpickling.
-- xpWrapEither :: (a -> Either String b, b -> a) -> PU t a -> PU t b
--
-- not to be confuesd with 'xpEither'
xpWrapEither :: (a -> Either String b) -> (b -> a) -> PU t a -> PU t b
xpWrapEither a2b b2a pua = PU {
        unpickleTree = \t -> case unpickleTree pua t of
            Right (val, rest) -> (flip (,) rest) <$> a2b val
            Left err  -> Left $ "xpWrapEither failed: "++err,
        pickleTree = \value -> pickleTree pua (b2a value)
    }

-- | Execute one of a list of picklers. The /selector function/ is used during pickling, and
-- the integer returned is taken as a 0-based index to select a pickler from /pickler options/.
-- Unpickling is done by trying each list element in order until one succeeds
-- (the /selector/ is not used).
--
-- This is typically used to handle each constructor of a data type. However, it
-- can be used wherever multiple serialization strategies apply to a single type.
xpAlt :: (a -> Int)  -- ^ selector function
      -> [PU t a]    -- ^ list of picklers
      -> PU t a
xpAlt selector picklers = PU {
        unpickleTree = doUnpickle,
        pickleTree = \value -> pickleTree (picklers !! (selector value)) value
    }
  where
    doUnpickle t =
        let tryAll [] = Left []
            tryAll (x:xs) =
                case unpickleTree x t of
                    Right r -> Right r
                    Left err  -> case tryAll xs of
                      Left errs -> Left $ err : errs
                      Right r -> Right r
        in case tryAll picklers of
          Left errs -> Left $ "In xpAlt:\n  " ++ concat (intersperse ";\n  " errs)
          Right r -> Right r


-- | Try the left pickler first and if that failes the right one.
-- wrapping the result in Left or Right, respectively
--
-- Not to be confued with 'xpWrapEither'
xpEither :: PU n t1 -> PU n t2 -> PU n (Either t1 t2)
xpEither xpl xpr = PU {
        unpickleTree = doUnpickle,
        pickleTree = \v -> case v of
          Left  l -> pickleTree xpl l
          Right r -> pickleTree xpr r
    }
  where
    doUnpickle t = case unpickleTree xpl t of
                    Right (val, rst) -> Right (Left val, rst)
                    Left err  -> case unpickleTree xpr t of
                      Right (val, rst) -> Right (Right val, rst)
                      Left _ -> Left $ "xpEither: both unpicklers failed"

-- | Pickler that during pickling always uses the first pickler, and during
-- unpickling tries the first, and on failure then tries the second.
xpTryCatch :: PU t a -> PU t a -> PU t a
xpTryCatch pu1 pu2 = PU
    { unpickleTree = \t -> case unpickleTree pu1 t of
             Right (val1, rest) -> Right (val1, rest)
             Left  err1 -> case unpickleTree pu2 t of
                 Right (val2, rest) -> Right (val2, rest)
                 Left  err2 -> Left $ "Both xpTryCatch picklers failed: <" ++ err1 ++ "> <" ++ err2 ++ ">"
    , pickleTree = pickleTree pu1
    }

-- | The zero pickler
--
-- Encodes nothing, always fails during unpickling. (Same as @'xpThrow' \"got xpZero\"@).
xpZero :: PU [t] a
xpZero = xpThrow "got xpZero"

-- | No output when pickling, always generates an error with the specified message on unpickling.
xpThrow :: String    -- ^ Error message
        -> PU [t] a
xpThrow msg = PU
  { unpickleTree = \t -> Left msg
  , pickleTree = const []
  }

-- | When unpickling check that all elements have been consumed after this
-- pickler, fails otherwise
--
-- When pickling, this is a noop
xpClean :: PU t a -> PU t a
xpClean xp = PU { unpickleTree = \x -> case unpickleTree xp x of
                     Left l -> Left l
                     Right (y, (Nothing, c )) -> Right (y, (Nothing, c ))
                     _ -> Left $ "xpClean: not clean"

                , pickleTree = pickleTree xp
                }

-- | When unpickling check that all elements have been consumed and
-- that the same is true for all nested picklers, fails otherwise
--
-- When pickling, this is a noop
xpRecursiveClean :: PU t a -> PU t a
xpRecursiveClean xp = PU { unpickleTree = \x -> case unpickleTree xp x of
                     Left l -> Left l
                     Right (_, (Just _, _)) ->
                       Left $ "xpRecursiveClean: not clean"
                     Right (_, (Nothing, False)) ->
                       Left $ "xpRecursiveClean: not recursive clean"
                     Right (y , (Nothing, True)) -> Right (y , (Nothing, True))
                     , pickleTree = pickleTree xp
                }


