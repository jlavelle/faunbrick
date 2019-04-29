module FaunBrick.AST.Util where

import Data.Monoid (Sum(..))

import FaunBrick.AST

groupBy :: (a -> a -> Bool) -> FaunBrick a -> FaunBrick (FaunBrick a)
groupBy _ Halt = Halt
groupBy p' (Loop as bs) = Loop (groupBy p' as) (groupBy p' bs)
groupBy p' (Instr x' xs') = Instr (Instr x' ys') zs'
  where
    (ys', zs') = go p' x' xs'
    go p z (Instr x xs)
      | p z x = (Instr x ys, zs)
      | otherwise = (Halt, Instr (Instr x ys) zs)
      where
        (ys, zs) = go p x xs
    go _ _ Halt = (Halt, Halt)
    go _ _ (Loop as bs) = (Halt, Loop (groupBy p' as) (groupBy p' bs))

group :: Eq a => FaunBrick a -> FaunBrick (FaunBrick a)
group = groupBy (==)

single :: a -> FaunBrick a
single a = Instr a Halt

instrSum :: Program -> Int
instrSum = getSum . foldMap (Sum . go)
  where
    go (Update n) = n
    go (Jump n)   = n
    go _ = 0
