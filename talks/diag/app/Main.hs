{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Data.Tree
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree

tfoldl :: Tree String
tfoldl = Node "+"
     [ Node "+"
       [ Node "+"
         [ Node "+"
           [ Node "z" []
           , Node "x1" []
           ]
         , Node "x2" []
         ]
       , Node "x3" []
       ]
     , Node "x4" []
     ]

tfoldr :: Tree String
tfoldr = Node "+"
     [ Node "x1" []
     , Node "+"
       [ Node "x2" []
       , Node "+"
         [ Node "x3" []
         , Node "+"
           [ Node "x4" []
           , Node "z" []
           ]
         ]
       ]
     ]

exampleSymmTree :: Diagram B
exampleSymmTree = vcat
  [ renderTree ((<> circle 1.2 # fc white) . text)
    (~~)
    (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) tfoldr)
    # centerXY # pad 1.2
  ]

main :: IO ()
main = mainWith exampleSymmTree
