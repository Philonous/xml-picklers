{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | XML pickler helpers.
--
-- Instead of writing picklers to tuples and then manually transforming those
-- into data types, we pickle to a heterogeneous list ('HList') and let
-- "GHC.Generics" convert it to and from the record for us.
--
-- == Worked example
--
-- Say we want to read this document:
--
-- > <contact id="ignored">
-- >   <name>Ada</name>
-- >   <email>ada@example.com</email>
-- >   <phone>555-0100</phone>
-- > </contact>
--
-- Declare a single-constructor record and derive 'Generic':
--
-- > data Contact = Contact
-- >   { contactName  :: Text
-- >   , contactEmail :: Maybe Text
-- >   , contactPhone :: Text
-- >   } deriving (Show, Generic)
--
-- Then give one child pickler per field, in declaration order, joined with
-- @(:::)@ and terminated by 'HNil':
--
-- > xpContact :: PU [Node] Contact
-- > xpContact =
-- >   xpRecord "contact" $
-- >         xpElemText "name"
-- >     ::: xpOption (xpElemText "email")
-- >     ::: xpElemText "phone"
-- >     ::: HNil
--
-- === Rules of thumb
--
-- * Exactly one pickler per field, in the same order as the record fields;
--   a mismatch shows up as an equality error on @PickleFields as ~ GHList (Rep b)@.
-- * The type must have a single constructor with at least one field.
-- * Attributes on the wrapping element are ignored.

module Data.XML.Pickle.Generic where

import Control.Monad (MonadPlus (..))
import Data.Kind
import Data.XML.Pickle
import Data.XML.Types
import GHC.Generics

-- Instead of writing picklers to tuples and then manually transforming those to
-- data types, we use heterogenous list plus machinery to
-- automatically convert to that.
infixr 8 :::

-- Helper HList
data HList (as :: [Type]) where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

instance Show (HList '[]) where
  show _ = "|"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  show (a ::: as) = "| " ++ show a ++ " " ++ show as

-- Isomorphism between data types and hlist
class GRecordHList f where
  type GHList f :: [Type]
  gToHList :: f p -> HList (GHList f)
  gFromHList :: HList (GHList f) -> f p

-- Strip metadata
instance (GRecordHList f) => GRecordHList (M1 i m f) where
  type GHList (M1 i m f) = GHList f
  gToHList = gToHList . unM1
  gFromHList = M1 . gFromHList

instance (GRecordHList b) => GRecordHList (S1 metas (Rec0 a) :*: b) where
  type GHList (S1 metas (Rec0 a) :*: b) = a ': GHList b
  gToHList (M1 (K1 a) :*: b) = a ::: gToHList b
  gFromHList (a ::: xs) =
    M1 (K1 a) :*: gFromHList xs

-- Field: singleton
instance GRecordHList (K1 i a) where
  type GHList (K1 i a) = '[a]
  gToHList (K1 x) = x ::: HNil
  gFromHList (x ::: HNil) = K1 x

-- structural picklers:
class XpHList (as :: [Type]) where
  type PickleFields as :: [Type]
  xpHList :: HList as -> PU [Node] (HList (PickleFields as))

instance XpHList '[] where
  type PickleFields '[] = '[]
  xpHList HNil =
    PU
      { pickleTree = \HNil -> [],
        unpickleTree = \_ -> return HNil
      }

instance (XpHList as) => XpHList (PU [Node] a ': as) where
  type PickleFields (PU [Node] a ': as) = a ': PickleFields as
  xpHList (pu ::: pus) =
    PU
      { pickleTree = \(x ::: xs) ->
          pickleTree pu x ++ pickleTree (xpHList pus) xs,
        unpickleTree = \r0 -> do
          (x, r1) <- getRest $ unpickleTree pu r0
          xs <- unpickleTree (xpHList pus) r1
          return (x ::: xs)
      }

xpRecord ::
  ( XpHList as,
    PickleFields as ~ GHList (Rep b),
    Generic b,
    GRecordHList (Rep b)
  ) =>
  Name ->
  HList as ->
  PU [Node] b
xpRecord name pus =
  xpWrap (to . gFromHList . snd) (((),) . gToHList . from) $
    xpElem name (xpClean xpUnit) (xpClean $ xpHList pus)

-- Empty data type
--
-- Fill every field with "mzero" (works with Maybe and lists at least)

class GEmpty f where
  gEmpty :: f a

instance (MonadPlus m) => GEmpty (K1 i (m a)) where
  gEmpty = K1 mzero

instance (GEmpty f, GEmpty g) => GEmpty (f :*: g) where
  gEmpty = gEmpty :*: gEmpty

instance (GEmpty f) => GEmpty (M1 i m f) where
  gEmpty = M1 gEmpty

empty :: (Generic a, GEmpty (Rep a)) => a
empty = to gEmpty

xpOptionOrEmpty :: (Generic a, GEmpty (Rep a)) => PU [t] a -> PU [t] a
xpOptionOrEmpty =
  xpWrap
    ( \case
        Nothing -> empty
        Just x -> x
    )
    Just
    . xpOption

xpElemNodesOrDefault :: Name -> a -> PU [Node] a -> PU [Node] a
xpElemNodesOrDefault name def =
  xpWrap
    ( \case
        Nothing -> def
        Just r -> r
    )
    Just
    . xpOption
    . xpWrap snd (\a -> ((), a))
    . xpElem name (xpClean xpUnit)
    . xpClean
