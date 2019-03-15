module Wax.MySemigroup where

import Prelude

--------------------------------------------------------------------------------
class MySemigroup a where
  (<!+!>) :: a -> a -> a

instance MySemigroup [a] where
  a <!+!> b = a ++ b

--------------------------------------------------------------------------------
newtype List' a = List' [a] deriving (Show, Eq)

instance MySemigroup (List' a) where
  (List' a) <!+!> (List' b) = List' $ b ++ a

--------------------------------------------------------------------------------
newtype IntAdd = IntAdd Int deriving (Show, Eq)
newtype IntMult = IntMult Int deriving (Show, Eq)

instance MySemigroup IntAdd where
  (IntAdd a) <!+!> (IntAdd b) = IntAdd $ a

instance MySemigroup IntMult where
  (IntMult a) <!+!> (IntMult b) = IntMult $ a * b
