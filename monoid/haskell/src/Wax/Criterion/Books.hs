{-# LANGUAGE QuasiQuotes #-}

module Wax.Criterion.Books where

--------------------------------------------------------------------------------
import Wax.Books

--------------------------------------------------------------------------------
import Criterion.Main
import Path
import Prelude
import Data.HashMap.Lazy

--------------------------------------------------------------------------------
main :: IO ()
main = go
  -- = defaultMain
  -- [ bgroup "readBooks" [ bench "default" $ nfIO go ]
  -- ]

--------------------------------------------------------------------------------
go :: IO ()
go = do
  books <- readBooks fp
  let (WordsMap wordsMap)  = foldMap toWordsMap books
  let leastUsed = findBy (curry $ (>5) . length . fst) (<) maxBound wordsMap
  let mostUsed  = findBy (curry $ (>5) . length . fst) (>) minBound wordsMap
  print leastUsed
  print mostUsed

--------------------------------------------------------------------------------
findBy :: (String -> v -> Bool) -> (v -> v -> Bool) -> v -> HashMap String v -> (String, v)
findBy f s e = foldrWithKey g ("", e)
  where g k v r@(_, vr) = if f k v && s v vr then (k, v) else r

--------------------------------------------------------------------------------
-- JUST FOR TESTING PURPOSES
fp :: Path Abs Dir
fp = [absdir|/Users/d12frosted/Developer/wax/monoid/scala/src/main/resources/books|]
