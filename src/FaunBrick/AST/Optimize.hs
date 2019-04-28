module FaunBrick.AST.Optimize where

import Data.List (group)

import FaunBrick.AST (Brick(..), FaunBrick)

type Optimization = FaunBrick -> FaunBrick

optimize :: Optimization
optimize = combineJumps
         . combineUpdates
         . makeClears

combineUpdates :: Optimization
combineUpdates = foldMap go . group
  where
    go [] = []
    go a@(x:_) = let l = length a in case x of
      Add    -> [ Update l ]
      Sub    -> [ Update (-l) ]
      Loop _ -> [ Loop $ combineUpdates brs | (Loop brs) <- a ]
      _      -> a

combineJumps :: Optimization
combineJumps = foldMap go . group
  where
    go [] = []
    go a@(x:_) = let l = length a in case x of
      Forward  -> [ Jump l ]
      Backward -> [ Jump (-l) ]
      Loop _   -> [ Loop $ combineJumps brs | (Loop brs) <- a ]
      _        -> a

makeClears :: Optimization
makeClears = fmap go
  where
    go op = case op of
      Loop ls -> case ls of
        [Sub] -> Clear
        [Add] -> Clear
        _     -> Loop $ makeClears ls
      _ -> op
