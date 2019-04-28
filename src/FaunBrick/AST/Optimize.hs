module FaunBrick.AST.Optimize where

import Data.List (groupBy)

import FaunBrick.AST (Brick(..), FaunBrick)

type Optimization = FaunBrick -> FaunBrick

optimize :: Optimization
optimize = combineUpdates

combineUpdates :: Optimization
combineUpdates = foldMap go . groupBy (==)
  where
    go [] = []
    go a@(x:_) = let l = length a in case x of
      Add    -> [ Update $ l  ]
      Sub    -> [ Update $ -l ]
      Loop _ -> [ Loop $ combineUpdates brs | (Loop brs) <- a ]
      _      -> a
