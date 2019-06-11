{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wax.Spec where

--------------------------------------------------------------------------------
import Wax.MySemigroup

--------------------------------------------------------------------------------
import Prelude
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------
mySemigroupProperties :: TestTree
mySemigroupProperties = testGroup "MySemigroup"
  [ testProperty "associativity for [Int]"
    (assocProp :: [Int] -> [Int] -> [Int] -> Bool)
  , testProperty "associativity for List' Int"
    (assocProp :: List' Int -> List' Int -> List' Int -> Bool)
  , testProperty "associativity for IntAdd"
    (assocProp :: IntAdd -> IntAdd -> IntAdd -> Bool)
  , testProperty "associativity for IntMult"
    (assocProp :: IntMult -> IntMult -> IntMult -> Bool)
  ]

assocProp :: (Eq a, MySemigroup a) => a -> a -> a -> Bool
assocProp x y z = (x <!+!> (y <!+!> z)) == ((x <!+!> y) <!+!> z)

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [mySemigroupProperties]

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = List' <$> arbitrary

instance Arbitrary IntAdd where
  arbitrary = IntAdd <$> arbitrary

instance Arbitrary IntMult where
  arbitrary = IntMult <$> arbitrary
