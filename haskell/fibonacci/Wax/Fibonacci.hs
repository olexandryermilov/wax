module Wax.Fibonacci where

--------------------------------------------------------------------------------
import Wax.Exp

--------------------------------------------------------------------------------
import Prelude
import Data.List (transpose)

--------------------------------------------------------------------------------
fibNaive :: Integer -> Integer
fibNaive 1 = 0
fibNaive 2 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

--------------------------------------------------------------------------------
fibTailR :: Integer -> Integer
fibTailR = go 0 1
  where go !a !b !n | n == 1    = a
                    | otherwise = go b (a + b) (n - 1)

--------------------------------------------------------------------------------
fibHs :: Integer -> Integer
fibHs n = fibs !! (fromIntegral n)
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--------------------------------------------------------------------------------
fibMatrix :: Integer -> Integer
fibMatrix = get . mexp (MatrixL2 (1, 1, 1, 0))
  where get (MatrixL2 (_, a, _, _)) = a

newtype MatrixL2 a = MatrixL2 (a, a, a, a)

instance Num a => Semigroup (MatrixL2 a) where
  MatrixL2 (a11, a12, a21, a22) <> MatrixL2 (b11, b12, b21, b22)
    = MatrixL2
    ( a11 * b11 + a12 * b21
    , a11 * b12 + a12 * b22
    , a21 * b11 + a22 * b21
    , a21 * b12 + a22 * b22)

instance Num a => Monoid (MatrixL2 a) where
  mempty = MatrixL2 (1, 0, 0, 1)
  mappend = (<>)

--------------------------------------------------------------------------------
fibMatrixWiki :: Integer -> Integer
fibMatrixWiki n = head (apply (Matrix [[0,1], [1,1]] ^ n) [0,1])

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

instance Num a => Num (Matrix a) where
  Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
  Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
  Matrix as * Matrix bs =
     Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
  negate (Matrix as) = Matrix (map (map negate) as)
  fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
  abs m = m
  signum _ = 1

apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]
