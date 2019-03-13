module Wax.Criterion.Fibonacci where

--------------------------------------------------------------------------------
import Wax.Fibonacci

--------------------------------------------------------------------------------
import Criterion.Main
import Prelude

--------------------------------------------------------------------------------
main :: IO ()
main
  = defaultMain
  [ bgroup "fibNaive"
    [ bench "40" $ whnf fibNaive 40
    ]

  , bgroup "fibTailR"
    [ bench "40"   $ whnf fibTailR 40
    , bench "1200" $ whnf fibTailR 1200
    ]

  , bgroup "fibHs"
    [ bench "40"   $ whnf fibHs 40
    , bench "1200" $ whnf fibHs 1200
    ]

  , bgroup "fibMatrix"
    [ bench "40"      $ whnf fibMatrix 40
    , bench "1200"    $ whnf fibMatrix 1200
    , bench "9999999" $ whnf fibMatrix 9999999
    ]

  , bgroup "fibMatrix (from Wiki)"
    [ bench "40"      $ whnf fibMatrixWiki 40
    , bench "1200"    $ whnf fibMatrixWiki 1200
    , bench "9999999" $ whnf fibMatrixWiki 9999999
    ]
  ]
