module Wax.Exp where

--------------------------------------------------------------------------------
import Data.Monoid
import Prelude

--------------------------------------------------------------------------------
{-# INLINABLE mexp #-}
{-# SPECIALISE mexp :: Sum Integer -> Integer -> Sum Integer #-}
mexp :: (Integral n, Monoid m) => m -> n -> m
mexp a0 n0 = f a0 n0
  where f a n | even n    = f (a <> a) (n `quot` 2)
              | n == 1    = a
              | otherwise = g (a <> a) ((n - 1) `quot` 2) a

        -- we need this helper function for tail recursion
        g a n b | even n    = g (a <> a) (n `quot` 2) b
                | n == 1    = a <> b
                | otherwise = g (a <> a) ((n - 1) `quot` 2) (b <> a)

--------------------------------------------------------------------------------
mexpNaive :: (Integral n, Monoid m) => m -> n -> m
mexpNaive a n = foldl (<>) mempty $ replicate (fromIntegral n) a
