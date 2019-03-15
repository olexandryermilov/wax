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
  [ testProperty "associativity for [Int]" $
      \x y z -> ((x :: [Int]) <!+!> (y <!+!> z)) == ((x <!+!> y) <!+!> z)
  , testProperty "associativity for List' Int" $
      \x y z -> ((x :: List' Int) <!+!> (y <!+!> z)) == ((x <!+!> y) <!+!> z)
  , testProperty "associativity for IntAdd" $
      \x y z -> ((x :: IntAdd) <!+!> (y <!+!> z)) == ((x <!+!> y) <!+!> z)
  , testProperty "associativity for IntMult" $
      \x y z -> ((x :: IntMult) <!+!> (y <!+!> z)) == ((x <!+!> y) <!+!> z)
  ]

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
