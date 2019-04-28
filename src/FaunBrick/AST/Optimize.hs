module FaunBrick.AST.Optimize where

import Data.Functor.Foldable (cata, embed)

import FaunBrick.AST.Util (group, single)
import FaunBrick.AST

type Optimization = Program -> Program

optimize :: Optimization
optimize = combineInstrs . makeClears

combineInstrs :: Optimization
combineInstrs = cata go . group
  where
    go :: FaunBrickF Program Program -> Program
    go HaltF = Halt
    go (InstrF g r)  = comb g <> r
    go (LoopF as bs) = Loop as bs

    comb :: Optimization
    comb a@(Instr x _) = let l = length a in case x of
      Add      -> single $ Update l
      Sub      -> single $ Update (-l)
      Forward  -> single $ Jump l
      Backward -> single $ Jump (-l)
      _        -> a
    comb _ = error "Optimizer: Unexpected input to comb."

makeClears :: Optimization
makeClears = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (Instr Add Halt) r) = Instr Clear r
    go (LoopF (Instr Sub Halt) r) = Instr Clear r
    go x = embed x
