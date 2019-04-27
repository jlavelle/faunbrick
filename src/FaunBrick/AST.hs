module FaunBrick.AST (Brick(..)) where

data Brick
  = Forward
  | Backward
  | Add
  | Sub
  | Put
  | Get
  | Loop [Brick]
  deriving Show
