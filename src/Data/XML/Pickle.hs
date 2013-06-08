{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE OverloadedStrings      #-}

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
--  * Most unpicklers will produce an error when their child unpicklers fail to consume all elements.
-- Use 'xpClean' to discard those elements
--
-- The data type @'PU' t a@ represents both a pickler (converting Haskell data
-- to XML) and an unpickler (XML to Haskell data), so your code only needs to be
-- written once for both serialization and deserialization.  The 'PU' primitives, such
-- as 'xpElem' for XML elements, may be composed into complex arrangements using
-- 'xpPair' and other combinators.
--
-- Most picklers will try to find the /first match/ rather than failing when
-- the first element doesn't match. This is why the target type often ist
-- a list. To prevent this behaviour and commit the pickler to the first
-- element available, use 'xpIsolate'.
--
-- The top level of the document does not follow this rule, because it is a single
-- node type.  'xpRoot' is needed to adapt this to type ['Node'] for your
-- pickler to use.  You would typically define a pickler for a whole document with
-- 'xpElem', then pickle it to a single 'Node' with @'pickleTree' (xpRoot myDocPickler) value@.
--
-- /NB/: Unresolved entities are considered an error and will trigger an exception
--
-- When unpickling, the folowing invariant regarding the list of remaining elements should be observed:
--
-- * The returned list should be a subset of or the initial list itself, that is, no elements should be added
-- or changed
--
-- * The relative order of elements should be preserved
--
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
-- > xpPeople :: PU [Node] [Person]
-- > xpPeople = xpElemNodes "people" $ xpAll xpPerson
-- >
-- > people = [
-- >     Person "Dave" 27 "A fat thin man with long short hair",
-- >     Person "Jane" 21 "Lives in a white house with green windows"]
-- >
-- > main = do
-- >     print $ pickle xpPeople people
--
-- Program outputs would be an xml-value equivalent to:
--
-- > <people>
-- >    <person name="Dave" age="27">A fat thin man with long short hair</person>
-- >    <person name="Jane" age="21">Lives in a white house with green windows</person>
-- > </people>
--
-- One of the selling points of this library is it's extensive error reporting.
-- For example let's examine what happens when we try to unpickle an XML value
-- that contains an error:
-- > <people>
-- >    <person nickname="Jimmy" age="21">Lives in a white house with green windows</person>
-- > </people>
-- Note the "nickname" attribute.
-- Unpickling this XML value will yield an UnpickleError:
-- > unpickle xpPeople wrongPersonXml
-- > Left (TraceStep ("xpElemNodes","\"people\"") (TraceStep ("children","") ...
-- For better readability we can format the error nicely with 'ppUnpickleError':
-- > ppUnpickleError . fromLeft $ unpickle xpPeople wrongPersonXml
-- > Error while unpickling:
-- >   -> xpElemNodes ("people")
-- >   -> children
-- >   -> xpAll
-- >   -> xpWrap
-- >   -> xpElem ("person")
-- >   -> attrs
-- >   -> xpPair
-- >   -> tuple (1)
-- >   Entity not found: "name"
--
--
-- Functions marked with /compat/ are included for
-- compatibility with hexpat-pickle

module Data.XML.Pickle (
  -- * Types
  PU(..)
  , Attribute
  , UnpickleResult(..)
  -- * Pickler Invocation
  , pickle
  , unpickle
   -- * Primitive picklers
  , xpUnit
  , xpZero
  , xpThrow
  , xpIso
  , xpPartial
   -- * Value-preserving picklers
  , xpId
  , xpFst
  , xpSnd
  , xpTrees
  , xpHead
  , xpTree
  , xpText0
  , xpText
  , xpString
  , xpRoot
  , xpPrim
  -- * XML specific picklers
  -- ** Attributes
  , xpAttribute
  , xpAttribute'
  , xpAttribute_
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
  , xpElemText
  , xpElemBlank
  , xpElemExists
  , xpElems
   -- ** Character Content
  , xpContent
  , xpBool
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
  , xpFindFirst
  , xpAll
  , xpSubsetAll
  , xpAllByNamespace
  , xpList0
  , xpSeqWhile
  , xpList
  -- *** Tuples
  -- | Tuple combinators apply their picklers from left to right
  , xp2Tuple
  , xpPair
  , (<#>)
  , xp3Tuple
  , xpTriple
  , xp4Tuple
  , xp5Tuple
  , xp6Tuple
  -- ** Wrappers
  -- *** value wrappers
  , xpWrap
  , xpConst
  , xpWrapEither
  , xpWrapMaybe
  , xpWrapMaybe_
  , xpAssert
  , xpMayFail
  , xpUnliftElems
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
  , xpClean
  -- * Error handling
  , UnpickleError(..)
  , ppUnpickleError
  , (<++>)
  , (<?+>)
  , dropTrace
  , (<?->)
  , (<?-->)
  , (<?>)
  , UnresolvedEntityException(..)
  -- * helper functions
  , flattenContent
  ) where

import           Control.Applicative (Applicative, (<$>), pure)
import           Control.Arrow
import qualified Control.Category as Cat
import           Control.Exception
import           Control.Monad
import           Data.Char (isSpace)
import           Data.Either
import           Data.List (partition)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid(Monoid, mempty, mappend)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Data.XML.Types

data UnpickleError = ErrorMessage Text
                   | TraceStep (Text, Text) UnpickleError
                   | Variants [UnpickleError]
                   deriving (Show, Typeable)

showTr (name, "") = Text.unpack name
showTr (name, extra) = concat [Text.unpack name , " (", Text.unpack extra, ")"]

printUPE (ErrorMessage m) = [Text.unpack m]
printUPE (TraceStep t es) = ("-> " ++ showTr  t) : printUPE es
printUPE (Variants vs) = concat
                       . zipWith (:) (map (\x -> show x ++ ")") [1..])
                       . (map $ map ( "  " ++))
                       $ (printUPE <$> vs)

ppUnpickleError :: UnpickleError -> String
ppUnpickleError e = "Error while unpickling:\n"
                      ++ (unlines $ map ("  " ++) (printUPE e))

instance Exception UnpickleError

data UnpickleResult t a = UnpickleError UnpickleError
                        | NoResult Text -- ^ Not found, description of element
                        | Result a (Maybe t) -- ^ Result and remainder. The
                                             -- remainder is wrapped in Maybe to
                                             -- avoid a Monoid constraint on t.
                                             --
                                             --  /Invariant/: When t is a
                                             -- Monoid, the empty remainder should
                                             -- always be @Nothing@ instead of
                                             -- @Just mempty@
                          deriving (Functor, Show)

instance Monad (UnpickleResult t) where
    return x = Result x Nothing
    Result x r >>= f = case f x of
        Result y r' -> Result y (if isJust r then r else r')
        y -> y
    UnpickleError e >>= _ = UnpickleError e
    NoResult e >>= _ = NoResult e


upe :: String -> UnpickleError
upe e = ErrorMessage (Text.pack e)

missing :: [Char] -> UnpickleError
missing e = upe $ "Entity not found: " ++ e


missingE :: [Char] -> UnpickleResult t a
missingE = UnpickleError . missing

leftoverE :: [Char] -> UnpickleResult t a
leftoverE l = UnpickleError . upe $ "Leftover Entities" ++ if null l then "" else
                                                             (": " ++ l)

child :: Show a => PU a b -> a -> UnpickleResult t b
child xp v = case unpickleTree xp v of
    UnpickleError e -> UnpickleError e
    NoResult e -> missingE $ Text.unpack e
    Result _ (Just es) -> leftoverE $ show es
    Result r Nothing -> Result r Nothing

child' :: PU t a -> t -> UnpickleResult s a
child' xp v = case unpickleTree xp v of
    UnpickleError e -> UnpickleError e
    NoResult e -> missingE $ Text.unpack e
    Result _ (Just es) -> leftoverE ""
    Result r Nothing -> Result r Nothing

leftover :: Maybe t -> UnpickleResult t ()
leftover t = Result () t

remList :: [t] -> Maybe [t]
remList [] = Nothing
remList xs = Just xs

mapUnpickleError f (UnpickleError e) = (UnpickleError $ f e)
mapUnpickleError _ x = x

data PU t a = PU
  { hints :: [(Text, Text)] -- ^ Describes the picklers. Each list element
                            -- describes one nested pickler (in case of composed
                            -- picklers). Each pair consists of the name of the
                            -- pickler and more identifying information
                            -- (e.g. the name of the expected element). The
                            -- outermost pickler comes first.
                            --
                            -- This information will be added to the error trace
                            -- by unpickleError.
  , unpickleTree :: t -> UnpickleResult t a -- ^ The unpickling function. You
                                            -- should use 'unpickle' instead
                                            -- since it will add the hints to
                                            -- the error trace
  , pickleTree :: a -> t -- ^ the pickle function.
  }

pu :: PU t a
pu = PU { hints = []
        , unpickleTree = const undefined
        , pickleTree = const undefined
        }

infixl 6 <++>
(<++>) :: (Text, Text) -> UnpickleError -> UnpickleError
(<++>) s e = TraceStep s e

infixl 6 <++.>
(<++.>) :: (Text, Text) -> UnpickleResult t a -> UnpickleResult t a
(<++.>) s = mapUnpickleError (s <++>)

infixr 0 <?>
infixr 0 <?->
infixr 0 <?-->

-- | Override the hints of a pickler
(<?>) :: (Text, Text) -> PU t a -> PU t a
(<?>) tr px = px{hints = [tr]}

(<??>) :: Text -> PU t a -> PU t a
(<??>) tr = (<?>) (tr, "")

-- | Drop one level of error traces
dropTrace :: PU t a -> PU t a
dropTrace xp = PU { hints = drop 1 $ hints xp
                  , pickleTree = pickleTree xp
                  , unpickleTree = \v -> case unpickleTree xp v of
                      UnpickleError (TraceStep _ e) -> UnpickleError e
                      x -> x
                  }

-- | Add a hint to a pickler
(<?+>) :: (Text, Text) -> PU t a -> PU t a
(<?+>) h xp = xp{ hints = h : hints xp
                , unpickleTree = \v -> case unpickleTree xp v of
                    UnpickleError e -> UnpickleError $ TraceStep h e
                    x -> x
                }

-- | Replace hint and drop one backtrace level on error
(<?->) :: (Text, Text) -> PU t a -> PU t a
(<?->) tr xp = tr <?> dropTrace xp

-- | Replace hint and drop two backtrace levels on error
(<?-->) :: (Text, Text) -> PU t a -> PU t a
(<?-->) tr xp = tr <?-> dropTrace xp

data UnresolvedEntityException = UnresolvedEntityException
                                   deriving (Typeable, Show)
instance Exception UnresolvedEntityException

ppName :: Name -> String
ppName (Name local ns pre) = let
  ns' = case ns of
    Nothing -> []
    Just ns'' -> ["{", Text.unpack ns'',"}"]
  pre' = case  pre of
    Nothing -> []
    Just pre'' -> [Text.unpack pre'',":"]
  in  concat . concat $ [["\""],ns', pre', [Text.unpack local], ["\""]]

-- | pickle a Tree
pickle :: PU t a -> a -> t
pickle = pickleTree

traceSteps hs e = foldr TraceStep e hs

-- | unpickle a tree
unpickle :: PU t a -> t -> Either UnpickleError a
unpickle xp x = case unpickleTree xp x of
    UnpickleError e -> Left e
    NoResult e -> Left . traceSteps (hints xp) . ErrorMessage $ "Entity not found " `Text.append` e
    Result r _ -> Right r

ppUnpickle xp x = case unpickle xp x of
    Right r -> Right r
    Left e -> Left $ ppUnpickleError e

for :: [a] -> (a -> b) -> [b]
for = flip map

mapLeft _ (Right r) = Right r
mapLeft f (Left l ) = Left $ f l

type Attribute = (Name,[Content])

-- | Isomorphic pickler
xpIso :: (a -> b) -> (b -> a) -> PU a b
xpIso f g = ("xpIso", "") <?+>
                        pu { unpickleTree = \t -> return $ f t
                           , pickleTree = g
                           }

xpPartial :: (a -> Either Text b)
          -> (b -> a)
          -> PU a b
xpPartial f g = ("xpEither", "") <?+>
               PU { pickleTree = g
                  , unpickleTree = \v -> case f v of
                      Left e -> UnpickleError $ ErrorMessage e
                      Right r -> Result r Nothing
                  }

-- | Doesn't create or consume anything, always succeeds
xpUnit :: PU [a] ()
xpUnit = ("xpUnit", "") <?+>
         pu { unpickleTree = \x -> leftover $ remList x
            , pickleTree = const []
            }

-- | Returns everything (remaining), untouched.
xpId :: PU a a
xpId = (xpIso id id){hints = []}

-- | 'xpId' (/compat/)
xpTrees :: PU a a
xpTrees = ("xpTree", "") <?> xpId

-- | Converts Booleans to XML boolean values
--
-- * true and 1 are read as True
--
-- * false and 0 are read as False
--
-- * all other values generate an unpickle error
--
-- Will always generate true or false (not 0 or 1) when pickling
xpBool :: PU Text Bool
xpBool = ("xpBool", "") <?+>
         pu { unpickleTree = \v -> case () of ()
                                                  | v `elem` ["true",  "1"] ->
                                                      Result True Nothing
                                                  | v `elem` ["false", "0"] ->
                                                      Result False Nothing
                                                  | otherwise -> UnpickleError
                                                                 (ErrorMessage $
                                                                  "Not a boolean value: "
                                                                  `Text.append` v)
            , pickleTree = \v -> case v of
                       True -> "true"
                       False -> "false"
            }

-- | Apply a bijection before pickling / after unpickling
xpWrap :: (a -> b) -> (b -> a) -> PU t a -> PU t b
xpWrap to from xp = ("xpWrap","") <?+>
                    pu { unpickleTree = \x -> to <$> unpickleTree xp x
                       , pickleTree = pickleTree xp . from
                       }

-- | Like xpWrap, but strips Just (and treats Nothing as a failure) during unpickling.
xpWrapMaybe :: (a -> Maybe b) -> (b -> a) -> PU t a -> PU t b
xpWrapMaybe a2b b2a pua = ("xpWrapMaybe","") <?>
                 xpWrapMaybe_ "xpWrapMaybe can't encode Nothing" a2b b2a pua

-- | Like xpWrap, but strips Just (and treats Nothing as a failure) during unpickling,
-- with specified error message for Nothing value.
xpWrapMaybe_ :: String -> (a -> Maybe b) -> ( b -> a) -> PU t a -> PU t b
xpWrapMaybe_ errorMsg a2b b2a pua =  PU {
        hints = [("xpWrapMaybe_","")]
        , unpickleTree = \t -> case unpickleTree pua t of
            Result val rest ->
                case a2b val of
                    Just val' -> Result val' rest
                    Nothing   -> UnpickleError $ upe errorMsg
            NoResult e -> NoResult e
            UnpickleError e  -> UnpickleError e
        , pickleTree = \value -> pickleTree pua (b2a value)
    }


-- | Lift a pickler. 'Nothing' is returned when the given pickler
-- doesn't return a value (e.g. the element isn't found). Does not affect
-- unpickling errors.
-- Nothing is pickled to []
--
-- A typical example is:
--
-- > xpElemAttributes "score" $ xpOption $ xpAttribute "value" xpPrim
--
-- in which @Just 5@ is encoded as @\<score value=\"5\"\/\>@ and @Nothing@
-- as @\<score\/\>@.
xpOption :: PU [t] a -> PU [t] (Maybe a)
xpOption pu = ("xpOption" ,"" ) <?+>
              pu { unpickleTree = doUnpickle
                 , pickleTree = \mValue ->
                      case mValue of
                          Just value -> pickleTree pu value
                          Nothing    -> mempty
                 }
  where
    doUnpickle t =
        case unpickleTree pu t of
            Result r t' -> Result (Just r) t'
            NoResult e -> Result Nothing (remList t)
            UnpickleError e -> UnpickleError e

-- | return one element, untouched
xpHead :: PU [a] a
xpHead = ("xpHead","") <?+>
         pu { unpickleTree = \t -> case t of
                   [] -> UnpickleError $ upe "No element remaining"
                   t:ts -> Result t (if null ts then Nothing else Just ts)
            , pickleTree = return
            }

-- | 'xpHead' (/compat/)
xpTree :: PU [a] a
xpTree = "xpTree" <??> xpHead

-- | specialised version of 'xpId' (/compat/)
xpText0 :: PU Text Text
xpText0 = ("xpText0", "") <?> xpId

-- | Convert text to/from String
xpString :: PU Text String
xpString = "xpString" <??> xpIso Text.unpack Text.pack

-- | Test predicate when unpickling. Fails with given error message when the
-- predicate return false.
--
-- N.B.: The predicate will only be tested while /unpickling/. When pickling,
-- this is a noop.
xpAssert :: Text -> (a -> Bool) -> PU t a -> PU t a
xpAssert err p xp = ("xpAssert", err) <?+>
                    pu { unpickleTree = \t -> do
                              r <- unpickleTree xp t
                              unless (p r) $ UnpickleError assertErr
                              return r
                       , pickleTree = pickleTree xp
                       }
  where
    assertErr = upe ("Assertion failed; " ++ Text.unpack err)

-- | Like 'xpText0', but fails on non-empty input.
xpText :: PU Text Text
xpText = ("xpText","") <?> xpAssert "Input is empty" (not . Text.null) xpText0

-- | Transforms a pickler on Lists to a pickler on single elements.
--
-- /N.B./ Will error when the given pickler doesn't produce exactly one element
xpRoot ::PU [a] b -> PU a b
xpRoot pa = ("xpRoot","") <?+>
    pu { unpickleTree = \t -> case unpickleTree pa [t] of
              Result x Nothing -> Result x Nothing
              Result x (Just _) -> UnpickleError $ upe "Leftover entities"
              UnpickleError e -> UnpickleError e
              NoResult e -> NoResult e
       , pickleTree = \t -> case pickleTree pa t of
           [t1] -> t1
           _    -> error "pickler called by xpRoot must output exactly one element"
       }

getFirst :: (t -> Bool) -> [t] -> Maybe (t, [t])
getFirst _ [] = Nothing
getFirst p (x:xs) = case p x of
      True  -> Just (x,xs)
      False -> (second (x:)) <$> getFirst p xs

-- | pickle to/from attribute
xpAttribute :: Name -> PU Text a -> PU [Attribute] a
xpAttribute name pu =  ("xpAttr" , Text.pack $ ppName name) <?+>
     pu { unpickleTree = doUnpickle
        , pickleTree = \value -> [(name, [ContentText $ pickleTree pu value])]
        }
  where
    doUnpickle attrs = case getFirst ((== name) . fst) attrs of
      Nothing -> NoResult $ Text.pack $ ppName name
      Just ((_,[ContentText x]), rem) -> case unpickleTree pu x of
        NoResult e -> missingE $ Text.unpack e
        UnpickleError e -> UnpickleError e
        Result _ (Just e) -> leftoverE $ show e
        Result r Nothing  -> Result r (remList rem)
      _ -> UnpickleError $ upe ("Unresolved entities in " ++ ppName name ++ ".")

-- | (/compat/)
xpAttr :: Name -> PU Text a -> PU [Attribute] a
xpAttr n xp = ("xpAttr", "") <?> xpAttribute n xp

-- | Pickle attribute if Just is given, on unpickling return Just <val> when
-- the attribute is found, Nothing otherwise
xpAttribute' :: Name -> PU Text a -> PU [Attribute] (Maybe a)
xpAttribute' name pu = ("xpAttribute'", "" ) <?> xpOption $ xpAttr name pu

xpAttrImplied :: Name -> PU Text a -> PU [Attribute] (Maybe a)
xpAttrImplied n xp = ("xpAttrImplied", "") <?> xpAttribute' n xp

-- | Pickle an attribute with the specified name and value, fail if the same attribute is
-- not present on unpickle.
xpAttribute_ :: Name -> Text -> PU [Attribute] ()
xpAttribute_ name val = ("xpAttribute_", "") <?->
    xpWrapMaybe_ ("expected fixed attribute "++ ppName name++"="++show val)
                (\v -> if v == val then Just () else Nothing) (const val) $
    xpAttr name xpId

xpAttrFixed :: Name -> Text -> PU [Attribute] ()
xpAttrFixed n t = ("xpAttrFixed", "") <?> xpAttribute_ n t

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
    addConcatText xs = let txt = Text.concat xs in
        if Text.all isSpace txt then id else  (nc txt :)

-- | When unpickling, tries to find the first element with the supplied name.
-- Once such an element is found, it will commit to it and /fail/ if any of the
-- picklers don't match.
xpElem :: Name -- ^ name of the Element
          -> PU [Attribute] a -- ^ pickler for attributes
          -> PU [Node] n  -- ^ pickler for child nodes
          -> PU [Node] (a,n)
xpElem name attrP nodeP =  tr <?+>
      pu { unpickleTree = doUnpickleTree
         , pickleTree   = \(a,n) -> [NodeElement $ Element name
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst (nodeElementNameHelper name) nodes of
      Just ((NodeElement (Element _ attrs children)), rem) -> do
          as <- ("attrs","") <++.> child attrP attrs
          cs <- ("children","") <++.> child nodeP (flattenContent children)
          leftover $ remList rem
          return (as, cs)
      _ -> NoResult $ Text.pack $ ppName name

    tr = ("xpElem", Text.pack $ ppName name)

    nodeElementNameHelper name (NodeElement (Element n _ _)) = n == name
    nodeElementNameHelper _ _ = False

-- | Handle all elements with a given name. The unpickler will fail when any of
-- the elements fails to unpickle.
xpElems :: Name -- ^ Name of the elements
        -> PU [Attribute] a -- ^ pickler for attributes
        -> PU [Node] n -- ^ pickler for child nodes
        -> PU [Node] [(a, n)]
xpElems name attrs children = tr <?-> xpSubsetAll isThisElem
                                       (xpElem name attrs children)
  where
    isThisElem (NodeElement (Element name' _ _)) = name' == name
    isThisElem _ = False

    tr = ("xpElems", Text.pack $ ppName name)

-- | Tries to apply the pickler to all the remaining elements;
-- fails if any of them don't match
xpAll :: PU [a] b -> PU [a] [b]
xpAll xp = ("xpAll", "") <?+>
           pu { unpickleTree = doUnpickleTree
              , pickleTree = \xs -> concatMap (pickleTree xp) xs
              } where
  doUnpickleTree xs = mapM (child' xp . return) xs

-- | For unpickling, apply the given pickler to a subset of the elements
-- determined by a given predicate
--
-- Pickles like 'xpAll'
xpSubsetAll :: (a -> Bool) -- ^ predicate to select the subset
            -> PU [a] b    -- ^ pickler to apply on the subset
            -> PU [a] [b]
xpSubsetAll pred xp = ("xpSubsetAll","") <?+>
                      pu { unpickleTree = \t ->
                            let (targets, rest) = partition pred t in
                            do
                                leftover $ remList rest
                                child' (xpAll xp) targets
                         , pickleTree = pickleTree (xpAll $ xp)
                         }


-- | Apply unpickler to all elements with the given namespace.
--
-- Pickles like 'xpAll'.
xpAllByNamespace :: Text -> PU [Node] b -> PU [Node] [b]
xpAllByNamespace namespace xp = ("xpAllByNamespace",namespace)
                                  <?> xpSubsetAll helper xp

  where
    helper (NodeElement (Element (Name _local (Just ns) _pre) _ _ ))
            = ns == namespace
    helper _ = False


-- | pickle Element without restriction on the name.
-- the name as taken / returned as the first element of the triple
xpElemWithName :: PU [Attribute] a  -- ^ pickler for attributes
                  -> PU [Node] n    -- ^ pickler for child nodes
                  -> PU [Node] (Name,a,n)
xpElemWithName attrP nodeP =  ("xpElemWithName", "") <?+>
      pu { unpickleTree = doUnpickleTree
         , pickleTree   = \(name, a,n) -> [NodeElement $ Element name
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst nodeElementHelper nodes of
      Just ((NodeElement (Element name attrs children)), rem) -> do
          x <- child attrP attrs
          y <- child nodeP $ flattenContent children
          leftover $ remList rem
          return (name, x, y)
      _ -> NoResult "element"
    nodeElementHelper (NodeElement (Element _ _ _)) = True
    nodeElementHelper _ = False

-- | find element by name space, prefixes are ignored
xpElemByNamespace :: Text -- ^ Namespace
                  -> PU Text name -- ^ Pickler for the local name
                  -> PU [Attribute] a  -- ^ pickler for attributes
                  -> PU [Node] n    -- ^ pickler for child nodes
                  -> PU [Node] (name,a,n)
xpElemByNamespace ns nameP attrP nodeP = ("xpElemByNamespace", ns) <?+>
      pu { unpickleTree = doUnpickleTree
         , pickleTree   = \(name, a,n) -> [NodeElement $ Element
                                     (Name (pickleTree nameP name) (Just ns) Nothing)
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst (nodeElementNSHelper ns) nodes of
      Just ((NodeElement (Element name attrs children)), rem) -> tr name $
          do
              name'  <- child nameP (nameLocalName name)
              attrs' <- child attrP attrs
              nodes' <- child nodeP children
              leftover $ remList rem
              return (name', attrs', nodes')

      _ -> NoResult $ "Element with namepspace " `Text.append` ns
    tr a res = case res of
        UnpickleError e -> UnpickleError (TraceStep
                                            ( "xpElemByNamespace found element"
                                            , Text.concat [nameLocalName a])
                                          e)
        x -> x

    nodeElementNSHelper ns (NodeElement (Element n _ _)) = nameNamespace n == Just ns
    nodeElementNSHelper ns _ = False

-- | Pickler Returns the first found Element untouched
--
-- Unpickler wraps element in 'NodeElement'
xpElemVerbatim ::  PU [Node] Element
xpElemVerbatim = ("xpElemVerbatim", "") <?+>
      pu { unpickleTree = doUnpickleTree
         , pickleTree   = \e -> [NodeElement e]
         } where
    doUnpickleTree nodes = case getFirst nodeElementHelper nodes of
      Just ((NodeElement e@(Element _ _ _)), rem) -> Result e (remList rem)
      _ -> NoResult "element"

    nodeElementHelper (NodeElement (Element _ _ _)) = True
    nodeElementHelper _ = False

-- | A helper variant of xpElem for elements that contain attributes but no child tags.
xpElemAttrs :: Name -> PU [Attribute] b -> PU [Node] b
xpElemAttrs name puAttrs = ("xpElemAttrs", Text.pack $ ppName name) <?->
                           xpWrap (fst) (\a -> (a,())) $
                             xpElem name puAttrs xpUnit

-- | A helper variant of xpElem for elements that contain child nodes but no attributes.
xpElemNodes :: Name -> PU [Node] b -> PU [Node] b
xpElemNodes name puChildren = ("xpElemNodes", Text.pack $ ppName name) <?->
                              xpWrap (snd) (\a -> ((),a)) $
                                xpElem name xpUnit puChildren

-- | A helper variant of xpElem for elements that contain only character data
xpElemText :: Name -> PU [Node] Text
xpElemText name = ("xpElemText", "") <?-> (xpElemNodes name $ xpContent xpId)

-- | Helper for Elements that don't contain anything
xpElemBlank :: Name -> PU [Node] ()
xpElemBlank name = ("xpElemBlank", "") <?-> xpWrap (const () ) (const ((),())) $
                                xpElem name xpUnit xpUnit

-- | When pickling, creates an empty element iff parameter is True
--
-- When unpickling, checks whether element exists. Generates an error when the
-- element is not empty
xpElemExists :: Name -> PU [Node] Bool
xpElemExists name = ("xpElemBlank", "") <?-->
                    xpWrap (\x -> case x of Nothing -> False; Just _ -> True)
                           (\x -> if x then Just () else Nothing) $
                           xpOption (xpElemBlank name)


-- | Get the first non-element NodeContent from a node
xpContent :: PU Text a -> PU [Node] a
xpContent xp = ("xpContent","") <?+>
    pu { unpickleTree = doUnpickle
       , pickleTree = return . NodeContent . ContentText . pickleTree xp
       } where
     doUnpickle nodes = case getFirst nodeContentHelper nodes of -- flatten
       Just ((NodeContent (ContentText t)), rem) -> child xp t
       Just ((NodeContent (ContentEntity t)), _) ->
           UnpickleError . upe $ "Unresolved entity" ++ show t ++ "."
       _ -> NoResult "node content"

     nodeContentHelper (NodeContent _) = True
     nodeContentHelper _ = False


-- | Unlift a pickler on Nodes to a Pickler on Elements. Nodes generated during
-- pickling that are not Elements will be silently discarded
xpUnliftElems :: PU [Node] a -> PU [Element] a
xpUnliftElems xp = ("xpUnliftElems","") <?+>
          pu { unpickleTree = doUnpickle
             , pickleTree = nodesToElems . pickleTree xp
             }
  where
    doUnpickle nodes = case unpickleTree xp (map NodeElement nodes) of
        UnpickleError e -> UnpickleError e
        NoResult e -> NoResult e
        Result a r -> let r' = case r of
                              Nothing -> Nothing
                              Just rs' -> case nodesToElems rs' of
                                  [] -> Nothing
                                  rs -> Just rs
                      in Result a r'
    nodesToElems = foldr (\n es -> case n of
                                      NodeElement e -> e : es
                                      _ -> es) []

-- | Optional conversion with default value
--
-- Unlike 'xpWithDefault' the default value is not encoded in the XML document,
-- during unpickling the default value is inserted if the pickler doesn't
-- returna a value
xpDefault :: (Eq a) => a -> PU [t] a -> PU [t] a
xpDefault df xp  = ("xpDefault", "") <?->
       xpWrap (fromMaybe df)
              (\ x -> if x == df then Nothing else Just x)
              (xpOption xp)

-- | Attempt to use a pickler. Return a default value when the pickler doesn't
-- return anything (Doesn't touch on UnpickleError)
--
-- Unlike 'xpDefault', the default value /is/ encoded in the XML document.
xpWithDefault :: a -> PU t a -> PU t a
xpWithDefault a pa = ("xpWithDefault","" ) <?+>
                     pu { pickleTree = pickleTree pa
                        , unpickleTree = \v -> case unpickleTree pa v of
                            Result r t -> Result r t
                            NoResult _ -> Result a (Just v)
                            UnpickleError e -> UnpickleError e
                        }


-- TODO:
-- We could use Monoid m => m instead of [a], but that is for another day...

-- | Try to extract the remainig elements, fail if there are none
getRest :: UnpickleResult [a] b -> UnpickleResult [a] (b, [a])
getRest (Result r (Just t)) = Result (r, t) Nothing
getRest (Result r Nothing) = Result (r, []) Nothing
getRest (NoResult e) = missingE $ Text.unpack e
getRest (UnpickleError e) = (UnpickleError e)

tErr :: Text -> UnpickleResult t a -> UnpickleResult t a
tErr tr = mapUnpickleError (("tuple", tr) <++>)

-- | Combines 2 picklers
xp2Tuple :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xp2Tuple xp1 xp2 = ("xp2Tuple", "") <?+>
                 pu { pickleTree = \(t1, t2) ->
                          pickleTree xp1 t1 ++ pickleTree xp2 t2
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    -- The /Either String/ monad
    (x1 ,r1) <- tErr "1" . getRest $ unpickleTree xp1 r0
    x2       <- tErr "2" $           unpickleTree xp2 r1
    return (x1,x2)


-- | 'xp2Tuple' (/compat/)
xpPair :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xpPair l r = ("xpPair", "") <?> xp2Tuple l r

-- | 'xp2Tuple'
(<#>) :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
(<#>) l r = ("(<#>)", "") <?> xp2Tuple l r

-- | Combines 3 picklers
xp3Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xp3Tuple xp1 xp2 xp3 =  ("xp3Tuple", "") <?+>
                        pu { pickleTree = \(t1, t2, t3) ->
                                 pickleTree xp1 t1
                                 ++ pickleTree xp2 t2
                                 ++ pickleTree xp3 t3
                           , unpickleTree = doUnpickleTree
                           } where
  doUnpickleTree r0 = do
    (x1, r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2, r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    x3       <- tErr "3" $           unpickleTree xp3 r2
    return (x1,x2,x3)

-- | 'xp3Tuple' (/compat/)
xpTriple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xpTriple l m r = "xpTriple" <??> xp3Tuple l m r

-- | Combines 4 picklers
xp4Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4
             -> PU [a] (a1, a2, a3,a4)
xp4Tuple xp1 xp2 xp3 xp4
    = ("xp4Tuple", "") <?+>
                 pu { pickleTree = \(t1, t2, t3, t4) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 =  do
    (x1 , r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2 , r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    (x3 , r3) <- tErr "3" $ getRest $ unpickleTree xp3 r2
    x4        <- tErr "4" $           unpickleTree xp4 r3
    return (x1,x2,x3,x4)

-- | Combines 5 picklers
xp5Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4 -> PU [a] a5
             -> PU [a] (a1, a2, a3, a4, a5)
xp5Tuple xp1 xp2 xp3 xp4 xp5
  = ("xp5Tuple", "") <?+>
                 pu { pickleTree = \(t1, t2, t3, t4, t5) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                        ++ pickleTree xp5 t5
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    (x1 , r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2 , r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    (x3 , r3) <- tErr "3" $ getRest $ unpickleTree xp3 r2
    (x4 , r4) <- tErr "4" $ getRest $ unpickleTree xp4 r3
    x5        <- tErr "5" $           unpickleTree xp5 r4
    return (x1,x2,x3,x4,x5)

-- | You guessed it ... Combines 6 picklers
xp6Tuple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4 -> PU [a] a5
             -> PU [a] a6
             -> PU [a] (a1, a2, a3, a4, a5, a6)
xp6Tuple xp1 xp2 xp3 xp4 xp5 xp6
  = ("xp6Tuple", "") <?+>
                 pu { pickleTree = \(t1, t2, t3, t4, t5, t6) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                        ++ pickleTree xp5 t5
                        ++ pickleTree xp6 t6
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    (x1 , r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2 , r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    (x3 , r3) <- tErr "3" $ getRest $ unpickleTree xp3 r2
    (x4 , r4) <- tErr "4" $ getRest $ unpickleTree xp4 r3
    (x5 , r5) <- tErr "5" $ getRest $ unpickleTree xp5 r4
    x6        <- tErr "6" $           unpickleTree xp6 r5
    return (x1,x2,x3,x4,x5,x6)

-- | When unpickling, don't consume the matched element(s), noop when pickling
xpPeek :: PU t a -> PU t a
xpPeek xp = ("xpPeek", "") <?+>
            pu { pickleTree = pickleTree xp
               , unpickleTree = \xs ->
                  case unpickleTree xp xs of
                    Result r _ -> Result r (Just xs)
                    x          -> x
               }

-- | Noop when pickling
--
-- When unpickling, only give access to the first element
xpIsolate :: PU [t] a -> PU [t] a
xpIsolate xp = ("xpIsolate","") <?+>
               pu { pickleTree = pickleTree xp
                  , unpickleTree = \xs -> case xs of
                      [] -> NoResult "entity"
                      (x:xs) -> case unpickleTree xp [x] of
                          Result r t -> Result r (remList $ mbToList t ++ xs)
                          NoResult e -> missingE $ Text.unpack e
                          x          -> x
                  } where
  handleRest r xs = case mbToList r ++ xs of [] -> Nothing; rs -> Just rs
  mbToList Nothing = []
  mbToList (Just r) = r

-- | Select a single element from the list and apply unpickler to it.
--
-- Returns no value when no element matches the predicate
--
-- Fails when the unpickler doesn't return a value
--
-- When pickling, this is a noop
xpFindFirst :: (t -> Bool) -> PU [t] a -> PU [t] a
xpFindFirst p xp = ("xpFindFirst","") <?+>
                   pu { pickleTree = pickleTree xp
                      , unpickleTree = \xs -> case break p xs of
                          (_, []) -> NoResult "entity"
                          (xs,y:ys) -> do
                              leftover . remList $ xs ++ ys
                              child' xp [y]
                      }

-- | Ignore input/output and replace with constant values
xpConst :: a -> PU t () -> PU t a
xpConst c xp = ("xpConst" ,"") <?> xpWrap (const c) (const ()) xp

-- | Convert text to/from any type that implements 'Read' and 'Show'.
-- Fails on unpickle if 'read' fails.
xpPrim :: (Show a, Read a) => PU Text a
xpPrim = ("xpPrim", "") <?+>
         pu { unpickleTree = \x -> case reads $ Text.unpack x of
                   []       -> UnpickleError $
                                 upe ("Couldn't read " ++ show x ++ ".")
                   (r,rest):_  -> Result r (Text.pack <$> remList rest)
            ,  pickleTree = Text.pack . show
            }

-- | When unpickling, tries to apply the pickler to all elements
-- returning and consuming only matched elements
xpFindMatches :: PU [b] a -> PU [b] [a]
xpFindMatches xp = ("xpFIndMatches", "") <?+>
                   pu { unpickleTree = doUnpickleTree
                      , pickleTree = \xs -> pickleTree xp =<< xs
                      } where
  doUnpickleTree xs =
    let (ls, rs) = partitionEithers . for xs $ \x ->
          case unpickleTree xp [x] of
            NoResult _ -> Left x
            Result r Nothing -> Right $ Result r Nothing
            Result r (Just _) -> Right $ leftoverE ""
            UnpickleError e -> Right $ UnpickleError e
        in leftover (remList ls) >> sequence rs

-- | 'xpAll' (/compat/)
xpList0 :: PU [a] b -> PU [a] [b]
xpList0 xp = "xpList0" <??> xpAll xp

-- | Like xpList, but only succeed during deserialization if at least a
-- minimum number of elements are unpickled.
xpListMinLen :: Int -> PU [a] b -> PU [a] [b]
xpListMinLen ml xp = ("xpListMinLen", Text.pack $ show ml) <?->
                    xpWrapEither testLength id (xpList xp)
  where
    testLength as
      | length as < ml = Left $ "Expecting at least " ++ show ml ++ " elements"
    testLength as = Right as


-- | When unpickling, sucessively applies pickler to single elements until it
-- doesn't return anything; returns all matched elements.
xpSeqWhile :: PU [a] b -> PU [a] [b]
xpSeqWhile pu = PU {
    hints = [("xpSeqWhile", "")]
    , unpickleTree = doUnpickle
    , pickleTree = concatMap $ pickleTree pu
    }
  where
    doUnpickle [] = Result [] Nothing
    doUnpickle es@(elt:rem) =
                case unpickleTree pu [elt] of
                    Result val _ -> case doUnpickle rem of
                                      Result xs r -> Result (val:xs) r
                                      e           -> e
                    NoResult _   -> Result [] (Just es)
                    UnpickleError e -> UnpickleError e

-- | 'xpSeqWhile' (/compat/)
xpList :: PU [a] b -> PU [a] [b]
xpList xp = "xpList" <??> xpSeqWhile xp

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
    = ("xpMap", Text.pack $ ppName en) <?--> (dropTrace .
                                    xpWrap M.fromList M.toList .
                                    xpAll $
                                      xpElem en
                                      (xpAttr an xpk)
                                      xpv
                                 )

-- | Like xpWrap, except it strips Right (and treats Left as a failure) during unpickling.
-- xpWrapEither :: (a -> Either String b, b -> a) -> PU t a -> PU t b
--
-- not to be confuesd with 'xpEither'
xpWrapEither :: Show e => (a -> Either e b) -> (b -> a) -> PU t a -> PU t b
xpWrapEither a2b b2a pua = PU {
    hints = [("xpWrapEither","")]
    , unpickleTree = \t -> case unpickleTree pua t of
            Result val rest -> case a2b val of
                Left e -> UnpickleError . upe  $ "Function returned Left "
                                                 ++ show e
                Right r -> Result r rest
            NoResult e -> NoResult e
            UnpickleError e -> UnpickleError e
        ,
        pickleTree = \value -> pickleTree pua (b2a value)
    }

-- | Execute one of a list of picklers. The /selector function/ is used during
-- pickling, and the integer returned is taken as a 0-based index to select a
-- pickler from /pickler options/.  Unpickling is done by trying each list
-- element in order until one returns a Result.  (the /selector/ is not used).
--
-- This is typically used to handle each constructor of a data type. However, it
-- can be used wherever multiple serialization strategies apply to a single type.
xpAlt :: (a -> Int)  -- ^ selector function
      -> [PU t a]    -- ^ list of picklers
      -> PU t a
xpAlt selector picklers =
    ("xpAlt", "") <?+>
   pu { unpickleTree = doUnpickle
      , pickleTree = \value -> pickleTree (picklers !! (selector value)) value
      }
  where
    eitherResult (Result r t) = Right (Result r t)
    eitherResult (UnpickleError e) = Left $ e
    eitherResult (NoResult e) = Left . missing $ Text.unpack e
    splitResults v = partitionEithers $ map (eitherResult . flip unpickleTree v)
                                     picklers
    doUnpickle v = case splitResults v of
        (_, (Result r t):_) -> Result r t
        (es, []) -> ("xpAlt", "") <++.> UnpickleError (Variants es)

-- | Try the left pickler first and if that doesn't produce anything the right
-- one.  wrapping the result in Left or Right, respectively
--
-- Not to be confued with 'xpWrapEither'
xpEither :: PU n t1 -> PU n t2 -> PU n (Either t1 t2)
xpEither xpl xpr = PU {
    hints = [("xpEither", "")]
    , unpickleTree = doUnpickle
    , pickleTree = \v -> case v of
          Left  l -> pickleTree xpl l
          Right r -> pickleTree xpr r
    }
  where
    doUnpickle t = case unpickleTree xpl t of
                    Result r t -> Result (Left r) t
                    NoResult e1 -> case unpickleTree xpr t of
                      Result r t -> Result (Right r) t
                      NoResult e2 -> UnpickleError $ ("xpEither","")
                                        <++> Variants [ missing $ Text.unpack e1
                                                      , missing $ Text.unpack e2
                                                      ]
                      UnpickleError e -> UnpickleError $ ("(xpEither)","Right")
                                           <++> e
                    UnpickleError e -> UnpickleError $ ("(xpEither)","Left")
                                           <++> e

-- | pickler that during pickling always uses the first pickler, and during
-- unpickling tries the first, and on failure then tries the second.
xpTryCatch :: PU t a -> PU t a -> PU t a
xpTryCatch pu1 pu2 = ("xpTryCatch", "") <?+>
 pu { unpickleTree = \t -> case unpickleTree pu1 t of
             Result val1 rest -> Result val1 rest
             NoResult e1 -> case unpickleTree pu2 t of
                 Result val2 rest -> Result val2 rest
                 NoResult e2 -> NoResult $ Text.concat [e1, " / ", e2]
                 UnpickleError e2 -> UnpickleError $ ("xpTryCatch","Right")
                                    <++> e2
             UnpickleError e1 -> case unpickleTree pu2 t of
                 Result val2 rest -> Result val2 rest
                 NoResult e2 -> UnpickleError
                                $ Variants [ e1
                                           , upe $ " / not found:"
                                                   ++ Text.unpack e2
                                           ]
                 UnpickleError e2 -> UnpickleError $ ("xpTryCatch","")
                                <++> Variants [e1, e2]
    , pickleTree = pickleTree pu1
    }

-- | The zero pickler
--
-- Encodes nothing, always fails during unpickling. (Same as @'xpThrow' \"got xpZero\"@).
xpZero :: Monoid m => PU m a
xpZero = ("xpZero","") <?> xpThrow "xpZero"

-- | No output when pickling, always generates an error with the specified message on unpickling.
xpThrow :: Monoid m
        => String    -- ^ Error message
        -> PU m a
xpThrow msg = ("xpThrow" , "") <?+>
             pu { unpickleTree = \t -> UnpickleError $ upe msg
                , pickleTree = const mempty
                }

-- | Add an attribute with a fixed value.
xpAddFixedAttr :: Name -> Text -> PU [Attribute] b -> PU [Attribute] b
xpAddFixedAttr name val pa
    = ("xpAddFixedAttr", Text.concat [Text.pack (ppName name), " = ", val]) <?->
      xpWrap snd ((,) ()) $
      xp2Tuple (xpAttrFixed name val) pa

xpFst :: Monoid b => PU t (a, b) -> PU t a
xpFst xp = "xpFst" <??> xpWrap fst (\x -> (x, mempty)) xp

xpSnd :: Monoid a => PU t (a, b) -> PU t b
xpSnd xp = "xpSnd" <??> xpWrap snd (\y -> (mempty, y)) xp

-- | Instead of failing the pickler will return no result
xpMayFail :: PU t a -> PU t a
xpMayFail xp = ("xpMayFail", "") <?+>
               pu { pickleTree = pickleTree xp
                  , unpickleTree = \v -> case unpickleTree xp v of
                      UnpickleError _ -> NoResult "failed with xpMayFail"
                      x -> x
                  }

-- | Run unpickler and consume and discard remaining elements
--
-- When pickling, this is a noop
xpClean :: PU t a -> PU t a
xpClean xp = ("xpClean", "") <?+>
             pu { unpickleTree = \x -> case unpickleTree xp x of
                     Result r _ -> Result r Nothing
                     e -> e
                , pickleTree = pickleTree xp
                }

instance Cat.Category PU where
    id = xpId
    g . f = PU { hints = hints g ++ hints f
               , pickleTree = pickleTree f . pickleTree g
               , unpickleTree = \val -> case unpickleTree f val of
                   Result resf rem -> case unpickleTree g resf of
                       UnpickleError e -> UnpickleError e
                       NoResult e -> NoResult e
                       Result _ (Just _) -> leftoverE ""
                       Result resg Nothing -> Result resg rem
                   NoResult e -> NoResult e
                   UnpickleError e -> UnpickleError $ traceSteps (hints g) e
               }
