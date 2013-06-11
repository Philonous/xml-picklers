{-# LANGUAGE  TemplateHaskell  #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.XML.Pickle.Tuples
  ( xpNTuple
  , tErr
  , getRest
  )
  where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle.Basic
import           Language.Haskell.TH

-- | Try to extract the remainig elements, fail if there are none
getRest :: UnpickleResult [a] b -> UnpickleResult [a] (b, [a])
getRest (Result r (Just t)) = Result (r, t) Nothing
getRest (Result r Nothing) = Result (r, []) Nothing
getRest (NoResult e) = missingE $ Text.unpack e
getRest (UnpickleError e) = UnpickleError e

tErr :: Text -> UnpickleResult t a -> UnpickleResult t a
tErr tr = mapUnpickleError (("tuple", tr) <++>)

-- | create n fresh names with a prefix
mkNames :: (Enum a, Num a, Show a) => [Char] -> a -> Q [Name]
mkNames prefix n = forM [1..n] $ \i -> newName (prefix ++ show i)

pickleTree' :: [Name] -> Int -> Q Exp
pickleTree' picklers n = do
    names <- mkNames "t" n
    -- \(t1, ... , tn) -> concat [ pickleTree xp1 t1
    --                           , ...
    --                           , pickleTree xpn tn
    --                           ]
    lamE [tupP $ map varP names ] $ [| concat $(pickles names) |]
  where
    pickles names = listE (zipWith (\xp t -> [| pickleTree $xp $t |])
                                              (varE <$> picklers)
                                              (varE <$> names))

unpickleTree' :: [Name] -> Int -> Q Exp
unpickleTree' picklers n = do
    r0 <- newName "r0"
    rNames <- mkNames "r" (n-1)
    xNames <- mkNames "x" (n)
    -- \r0 -> do
    --     ...                                                       --\
    --     (xi , ri) <- tErr "i" $ getRest $ unpickleTree xpi r(i-1)    > binds
    --     ...                                                       --/
    --     xn        <- tErr "n"           $ unpickleTree xpn r(n-1) => lastbind
    --     return (x1, ... , xn)                                     => returnXs
    lamE [varP r0] $ doE $ binds r0 rNames xNames
                      ++ [ bindLast (last picklers) (last xNames) (last rNames)
                         , returnXs (varE <$> xNames)
                         ]

  where
    binds r0 rNames xNames = zipWith5 pickleLine (varP <$> xNames)
                                                 (varP <$> rNames)
                                                 ([1..] :: [Int])
                                                 picklers
                                                 (varE <$> r0 : rNames)
    pickleLine x r i xp rp = bindS (tupP [x, r])
                    [| tErr $(stringE $ show (i :: Int)) $ getRest
                       $ unpickleTree $(varE xp) $rp |]
    bindLast xp xn rnminus1 = bindS (varP xn)
                    [| tErr $(stringE $ show (n :: Int))
                       $ unpickleTree $(varE xp) $(varE rnminus1) |]
    returnXs xNames = noBindS [| return $(tupE xNames) |]

-- xpNTuple :: Int -> Q Exp

xpNTuple :: Int -> Q [Dec]
xpNTuple n = do

    let name = "xp" ++ show n ++ "Tuple"
        decName = mkName $ name

    --
    -- Type declaration
    --
        tNames = (\i -> mkName ("b" ++ show i)) <$> [1..n]
    -- (b1, b2, ... bn)
        resultTupleType = (foldl' appT (tupleT n) (varT <$> tNames))
        a = varT $ mkName "a"
    -- PU [a] [(b1, ..., bn)
        resultType = [t| PU [ $a ] $resultTupleType |]
    -- PU [a] b1 -> ... -> PU [a] bn -> PU [a] (b1, ..., bn)
        picklerType = foldr appT resultType
              (appT arrowT . (\bi -> [t| PU [ $a ] $(varT bi) |]) <$> tNames)
    signature <- sigD decName $ forallT (PlainTV <$> mkName "a" : tNames)
                                        (return [])
                                        picklerType

    --
    -- Function Body
    --
    xpNames <- mkNames "xp" n
    let body = [| $(stringE name) <??>
                  PU { pickleTree = $(pickleTree' xpNames n)
                     , unpickleTree = $(unpickleTree' xpNames n)
                     }
               |]
    dec <- funD decName [clause (varP <$> xpNames)
                         (normalB body)
                         []
                 ]
    return [signature, dec]
