module FaunBrick.AST.Optimize where

import Data.Functor.Foldable (cata, embed)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

import FaunBrick.AST.Util (groupBy, single, instrSum, arithmetic, instrEq)
import FaunBrick.AST

type Optimization = Program -> Program

optimize :: Optimization
optimize = mulLoop . combineInstrs . makeClears

combineInstrs :: Optimization
combineInstrs = cata go . groupBy instrEq
  where
    go :: FaunBrickF Program Program -> Program
    go HaltF = Halt
    go (InstrF g r)  = comb g <> r
    go (LoopF as bs) = Loop as bs

    comb :: Optimization
    comb a@(Instr x _) = case x of
      Update _ -> single $ Update $ instrSum a
      Jump _   -> single $ Jump   $ instrSum a
      _        -> a
    comb _ = error "Optimizer: Unexpected input to comb."

makeClears :: Optimization
makeClears = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (Instr (Update _) Halt) r) = Instr Clear r
    go x = embed x

mulLoop :: Optimization
mulLoop = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go x@(LoopF l r) = case arithmetic l >>= hasMul of
      Just i  -> i <> single Clear <> r
      Nothing -> embed x
    go x = embed x

hasMul :: [Instruction] -> Maybe Program
hasMul = analyze . sim

sim :: [Instruction] -> (IntMap Int, Int)
sim = foldl go (mempty, 0)
  where
    go (m, p) = \case
      Update n -> (M.insertWith (+) p n m, p)
      Jump n   -> (m, p + n)
      _ -> error "Optimizer: sim given non-arithmetic instruction"

analyze :: (IntMap Int, Int) -> Maybe (Program)
analyze (m, p)
  | p == 0 && M.lookup 0 m == Just (-1) = Just prog
  | otherwise = Nothing
  where
    prog = M.foldMapWithKey (\k v -> single $ Mul k v) (M.delete 0 m)
