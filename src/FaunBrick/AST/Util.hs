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
      Set _ n -> n
      _ -> 0

addOffset :: Int -> Instruction -> Instruction
addOffset x = \case
  Output o -> Output $ o + x
  Input o  -> Input $ o + x
  Update o n -> Update (o + x) n
  Set o n -> Set (o + x) n
  MulUpdate s d n -> MulUpdate (s + x) (d + x) n
  MulSet s d n -> MulSet (s + x) (d + x) n
  Jump n -> Jump n

-- Eq that ignores the numeric values contained in the instructions and compares offsets
instrEq :: Instruction -> Instruction -> Bool
instrEq a b = toConstr a == toConstr b && offsets a == offsets b

offsets :: Instruction -> Maybe (Either Int (Int, Int))
offsets = \case
  Output o -> Just $ Left o
  Input o -> Just $ Left o
  Update o _ -> Just $ Left o
  Set o _ -> Just $ Left o
  MulUpdate s d _ -> Just $ Right (s, d)
  MulSet s d _ -> Just $ Right (s, d)
  Jump _ -> Nothing

-- Test if a program only contains arithmetic operations (Update/Jump)
arithmetic :: Program -> Maybe Program
arithmetic = cata go
  where
    go :: FaunBrickF Instruction (Maybe Program) -> Maybe Program
    go HaltF = Just Halt
    go (InstrF i@(Update _ _) r) = Instr i <$> r
    go (InstrF i@(Jump _) r) = Instr i <$> r
    go _ = Nothing
