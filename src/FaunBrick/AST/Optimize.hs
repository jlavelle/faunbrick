module FaunBrick.AST.Optimize where

import Data.Functor.Foldable (cata, embed)

import FaunBrick.AST.Util (group, single, instrSum)
import FaunBrick.AST

type Optimization = Program -> Program

optimize :: Optimization
optimize = makeClears . combineInstrs

combineInstrs :: Optimization
combineInstrs = cata go . group
  where
    go :: FaunBrickF Program Program -> Program
    go HaltF = Halt
    go (InstrF g r)  = comb g <> r
    go (LoopF as bs) = Loop as bs

    comb :: Optimization
    comb a@(Instr x _) = let l = length a in case x of
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
