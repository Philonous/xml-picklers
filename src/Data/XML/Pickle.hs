{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
            UndecidableInstances, FunctionalDependencies, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | /xml-types-pickle/ provides XML picklers that plug into the xml tree of the
-- /xml-types/ package.
-- This module was "inspired" by hexpat-pickle
-- The API differences between /hexpat-pickle/ and /xml-types-pickle/ include:
--
--  * When unpickling, picklers will /consume/ matching elmements so that they will
--    be ignored by sucessive picklers
--  To circumvent this behaviour, use @'xpPeek'
--
--  * xpWrapper is uncurried
--
--  * There are no lazy unpicklers
--
-- The data type @'PU' t a@ represents both a pickler (converting Haskell data
-- to XML) and an unpickler (XML to Haskell data), so your code only needs to be
-- written once for both serialization and deserialization.  The 'PU' primitives, such
-- as 'xpElem' for XML elements, may be composed into complex arrangements using
-- 'xpPair' and other combinators.
--
-- The @t@ argument represents the part of the XML tree
-- that this 'PU' works on.
--
-- The reason why you a list of 'Node' instead of just one when working with a single
-- element is because the unpickler of 'xpElem' needs to see the whole list of nodes
-- so that it can 1. skip whitespace, and 2. search to match the specified tag name.
--
-- The top level of the document does not follow this rule, because it is a single
-- 'Node' type.  'xpRoot' is needed to adapt this to type ['Node'] for your
-- pickler to use.  You would typically define a pickler for a whole document with
-- 'xpElem', then pickle it to a single 'Node' with @'pickleTree' (xpRoot myDocPickler) value@.
--
-- The type for /text content/ works for attribute values directly, but if you want
-- to use it as the text content of an element, you need to adapt it by wrapping with
-- 'xpContent'.
--
--
--
-- Here is a simple and complete example to get you started:
--
-- >-- Person name, age and description
-- >import Data.Text
-- >import Data.XML.Types
-- >import Data.XML.Pickle
-- >data Person = Person Text Int Text
-- >
-- >xpPerson :: PU [Node] Person
-- >xpPerson =
-- >    -- How to wrap and unwrap a Person
-- >    xpWrap (\((name, age), descr) -> Person name age descr)
-- >           (\(Person name age descr) -> ((name, age), descr)) $
-- >    xpElem "person"
-- >        (xpPair
-- >            (xpAttr "name" xpId)
-- >            (xpAttr "age" xpPrim))
-- >        (xpContent xpId)
-- >
-- >people = [
-- >    Person "Dave" 27 "A fat thin man with long short hair",
-- >    Person "Jane" 21 "Lives in a white house with green windows"]
-- >
-- >main = do
-- >    print $ pickle (xpRoot $ xpElemNodes "people" $ xpAll xpPerson) people
--
-- Program outputs something equivalent to:
--
-- > <?xml version="1.0" encoding="UTF-8"?>
-- > <people><person name="Dave" age="27">A fat thin man with long short hair</person>
-- > <person name="Jane" age="21">Lives in a white house with green windows</person></people>
--
-- Funktions marked with /compat/ are included for compatibility with hexpat-pickle

module Data.XML.Pickle where

import Control.Applicative ((<$>))
import Control.Arrow


import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Data.XML.Types

-- | The pickler data type
--
-- [@clean@] an unpickling is considered @clean@ when it doesn't leave any remainng elements
-- [@recursively clean@] an unpickling is considered @recursively clean@ if it and any nested picklers are clean
--

data PU t a = PU
  { unpickleTree :: t -> Either String ( a -- ^ the return value
                                       , ( Maybe t -- ^ remaining elements
                                         , Bool -- ^ are we recursively clean
                                         )
                                       )
  , pickleTree :: a -> t
  }

pickle :: PU t a -> a -> t
pickle = pickleTree

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

xpElem :: Name -> PU [Attribute] a -> PU [Node] n -> PU [Node] (a,n)
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
          Right (x,(_,ca)) -> case unpickleTree nodeP children of
            Left e -> Left $ "in element " ++ show name ++ " : " ++ e
            Right (y,(_,cc)) ->
              let rem' = if null rem then Nothing else Just rem
              in Right ((x,y), (rem' , ca && cc))
      _ -> Left $ "xpElem: " ++ show name ++ " not found."

    nodeElementNameHelper name (NodeElement (Element n _ _)) = n == name
    nodeElementNameHelper _ _ = False

xpElemWithName :: PU [Attribute] a -> PU [Node] n -> PU [Node] (Name,a,n)
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
          Right (x,(_,ca)) -> case unpickleTree nodeP children of
            Left e -> Left $ "in element " ++ show name ++ " : " ++ e
            Right (y,(_,cc)) ->
              let rem' = if null rem then Nothing else Just rem
              in Right ((name,x,y), (rem' , ca && cc))
      _ -> Left $ "xpAnyElem: no element not found."

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
--
-- Note on lazy unpickle: The argument is evaluated strictly.
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
--
-- Note on lazy unpickle: The child is evaluated strictly.
xpWithDefault :: a -> PU t a -> PU t a
xpWithDefault a pa = xpTryCatch pa (lift a)
  where
    -- use this instead of standard xpLift, allowing us to use a more general tree type
    lift a = PU
      { unpickleTree = \t -> Right (a, (Just t, True))
      , pickleTree = error "xpWithDefault impossible" -- xpTryCatch never runs the second pickler
      }

getRest (_, (Nothing, _)) = Left $ "Not enough elements"
getRest (l, (Just r , c)) = Right (l,(r, c))


xpUnit :: PU [a] ()
xpUnit = PU (\x -> Right ((), (Just x, True))) (const [])

xp2Tuple :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xp2Tuple xp1 xp2 = PU {pickleTree = \(t1, t2) ->
                        pickleTree xp1 t1 ++ pickleTree xp2 t2
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp2Tuple: " ++) $ do
    (x1 ,(r1,c1)) <- getRest =<< unpickleTree xp1 r0
    (x2 ,(r ,c2)) <-             unpickleTree xp2 r1
    return ((x1,x2),(r,c1 && c2))

xpPair :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xpPair = xp2Tuple

xp3Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xp3Tuple xp1 xp2 xp3 = PU {pickleTree = \(t1, t2, t3) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = mapLeft ("In xp3Tuple: " ++) $ do
    (x1 ,(r1,c1)) <- getRest =<< unpickleTree xp1 r0
    (x2 ,(r2,c2)) <- getRest =<< unpickleTree xp2 r1
    (x3 ,(r ,c3)) <-             unpickleTree xp3 r2
    return ((x1,x2,x3),(r, c1 && c2 && c3))

xpTriple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xpTriple = xp3Tuple

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
    (x1 ,(r1, c1)) <- getRest =<< unpickleTree xp1 r0
    (x2 ,(r2, c2)) <- getRest =<< unpickleTree xp2 r1
    (x3 ,(r3, c3)) <- getRest =<< unpickleTree xp3 r2
    (x4 ,(r , c4)) <-             unpickleTree xp4 r3
    return ((x1,x2,x3,x4),(r, c1 && c2 && c3 && c4))

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
    (x1 ,(r1,c1)) <- getRest =<< unpickleTree xp1 r0
    (x2 ,(r2,c2)) <- getRest =<< unpickleTree xp2 r1
    (x3 ,(r3,c3)) <- getRest =<< unpickleTree xp3 r2
    (x4 ,(r4,c4)) <- getRest =<< unpickleTree xp4 r3
    (x5 ,(r ,c5))  <-             unpickleTree xp5 r4
    return ((x1,x2,x3,x4,x5),(r, c1 && c2 && c3 && c4 && c5))

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
    (x1 ,(r1,c1)) <- getRest =<< unpickleTree xp1 r0
    (x2 ,(r2,c2)) <- getRest =<< unpickleTree xp2 r1
    (x3 ,(r3,c3)) <- getRest =<< unpickleTree xp3 r2
    (x4 ,(r4,c4)) <- getRest =<< unpickleTree xp4 r3
    (x5 ,(r5,c5)) <- getRest =<< unpickleTree xp5 r4
    (x6 ,(r ,c6))  <- {- return-} unpickleTree xp6 r5
    return ((x1,x2,x3,x4,x5,x6),(r, c1 && c2 && c3 && c4 && c5 && c6))

-- | When unpickling, don't consume the matched element(s)
xpPeek :: PU t a -> PU t a
xpPeek xp = PU { pickleTree = pickleTree xp
               , unpickleTree = \xs ->
                  case unpickleTree xp xs of
                    Left e -> Left e
                    Right (r,(_,c)) -> Right (r,(Just xs,c))
               }

xpWrap :: (a -> b) -> (b -> a) -> PU t a -> PU t b
xpWrap to from xp = PU { unpickleTree = \x -> (first to) <$> unpickleTree xp x
                       , pickleTree = pickleTree xp . from
                       }

-- | Convert XML text content \<-\> any type that implements 'Read' and 'Show'.
-- Fails on unpickle if 'read' fails.
xpPrim :: (Show a, Read a) => PU Text a
xpPrim = PU { unpickleTree = \x -> case reads $ Text.unpack x of
                 []       -> Left $ "In xpPrim: couldn't read " ++ show x ++ "."
                 (r,_):_  -> Right r
            ,  pickleTree = Text.pack . show
            }


-- | find all remaining elements in the remaining set
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

-- | tries to apply the pickler to all the remaining elements
-- fails if any of them don't match
xpAll :: PU [a] b -> PU [a] [b]
xpAll xp = PU { unpickleTree = \x ->
                 let Right (xs, (rs,c)) = unpickleTree (xpFindMatches xp) x
                   in case rs of
                      Just (_:_) -> Left "In xpAll: not all Elements matched"
                      _ -> Right (xs, (Nothing,c))
              , pickleTree = \xs -> pickleTree xp =<< xs
              }

-- | (/compat/)
xpList :: PU [a] b -> PU [a] [b]
xpList = xpAll

-- | Standard pickler for maps
--
-- This pickler converts a map into a list of pairs of the form
--
-- > <elt attr="key">value</elt>
xpMap :: Ord k =>
     Name -> -- ^ Element name (elt)
     Name -> -- ^ Attribute name (attr)
     PU Text k -> -- ^ Pickler for keys (key)
     PU [Node] a -> -- ^ Pickler for values (value)
     PU [Node] (M.Map k a)
xpMap en an xpk xpv
    = xpWrap M.fromList
             M.toList
              $
      xpAll $
      xpElem en
          (xpAttr an xpk)
          xpv

-- | Like xpWrap, but strips Just (and treats Nothing as a failure) during unpickling.
xpWrapMaybe :: (a -> Maybe b, b -> a) -> PU t a -> PU t b
xpWrapMaybe = xpWrapMaybe_ "xpWrapMaybe can't encode Nothing value"

-- | Like xpWrap, but strips Just (and treats Nothing as a failure) during unpickling,
-- with specified error message for Nothing value.
xpWrapMaybe_ :: String -> (a -> Maybe b, b -> a) -> PU t a -> PU t b
xpWrapMaybe_ errorMsg (a2b, b2a) pua = PU {
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
xpWrapEither (a2b, b2a) pua = PU {
        unpickleTree = \t -> case unpickleTree pua t of
            Right (val, rest) -> a2b (val, rest)
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
        let tryAll [] = Left "all xpAlt unpickles failed"
            tryAll (x:xs) =
                case unpickleTree x t of
                    Right (val, rest) -> Right (val, rest)
                    Left err  -> tryAll xs
        in  tryAll picklers

-- | Pickler that during pickling always uses the first pickler, and during
-- unpickling tries the first, and on failure then tries the second.
--
-- Note on lazy unpickle: The first argument is evaluated strictly.
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
-- Encodes nothing, fails always during unpickling. (Same as @'xpThrow' \"got xpZero\"@).
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
-- pickler
xpClean :: PU t a -> PU t a
xpClean xp = PU { unpickleTree = \x -> case unpickleTree xp x of
                     Left l -> Left l
                     Right (y, (Nothing, c )) -> Right (y, (Nothing, c ))
                     _ -> Left $ "xpClean: not clean"

                , pickleTree = pickleTree xp
                }

-- | When unpickling check that all elements have been consumed and
-- that the same is true for all nested picklers
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


