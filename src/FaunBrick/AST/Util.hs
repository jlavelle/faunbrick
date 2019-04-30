module FaunBrick.AST.Util where

import Data.Data (toConstr)
import Data.Monoid (Sum(..))
import Data.Functor.Foldable (cata)

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
    go = \case
      Update _ n -> n
      Jump n -> n
      _ -> 0

-- Eq that ignores the numeric values contained in the instructions and compares offsets
instrEq :: Instruction -> Instruction -> Bool
instrEq a b = toConstr a == toConstr b && offsets a == offsets b

offsets :: Instruction -> Maybe (Either Int (Int, Int))
offsets = \case
  Put o -> Just $ Left o
  Get o -> Just $ Left o
  Update o _ -> Just $ Left o
  Set o _ -> Just $ Left o
  MulUpdate s d _ -> Just $ Right (s, d)
  MulSet s d _ -> Just $ Right (s, d)
  Jump _ -> Nothing

-- Test if a program only contains arithmetic operations (Update/Jump)
arithmetic :: Program -> Maybe [Instruction]
arithmetic = cata go
  where
    go :: FaunBrickF Instruction (Maybe [Instruction]) -> Maybe [Instruction]
    go HaltF = Just []
    go (InstrF i@(Update _ _) r) = (i:) <$> r
    go (InstrF i@(Jump _) r)   = (i:) <$> r
    go _ = Nothing
