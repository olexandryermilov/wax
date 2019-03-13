{-# LANGUAGE QuasiQuotes #-}

module Wax.Criterion.Books where

--------------------------------------------------------------------------------
import Wax.Books

--------------------------------------------------------------------------------
import Criterion.Main
import Path
import Path.IO
import Prelude
import Data.HashMap.Lazy

--------------------------------------------------------------------------------
main :: IO ()
main
  = defaultMain
  [ bgroup "readBooks" [ bench "default" $ nfIO go ]
  ]

--------------------------------------------------------------------------------
go :: IO ()
go = do
  fp <- getCurrentDir
  books <- readBooks $ parent fp </> [reldir|scala/src/main/resources/mapreduce/books/|]
  let (WordsMap wordsMap)  = foldMap toWordsMap books
  let leastUsed = findBy (curry $ (>5) . length . fst) (<) maxBound wordsMap
  let mostUsed  = findBy (curry $ (>5) . length . fst) (>) minBound wordsMap
  print leastUsed
  print mostUsed

--------------------------------------------------------------------------------
findBy :: (String -> v -> Bool) -> (v -> v -> Bool) -> v -> HashMap String v -> (String, v)
findBy f s e = foldrWithKey g ("", e)
  where g k v r@(_, vr) = if f k v && s v vr then (k, v) else r
