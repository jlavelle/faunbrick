module Main where

import Criterion.Main
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LT

import FaunBrick.AST (FaunBrick)
import FaunBrick.Parser (parseFaunBrick, parseFile)
import qualified FaunBrick.Interpreter.Pure as Pure

main :: IO ()
main = defaultMain
  [ env setupParseEnv $ \ ~(simple, fib, lorem) -> bgroup "Parser"
    [ bench "parse simple.b" $ whnf parseFaunBrick simple
    , bench "parse fib.b"    $ whnf parseFaunBrick fib
    , bench "parse lorem.b"  $ whnf parseFaunBrick lorem
    ]
  , env setupInterpEnv $ \lorem -> bgroup "Pure Interpreter"
    [ bench "interpret lorem.b" $ whnf interpretPure lorem
    ]
  ]

setupParseEnv :: IO (Text, Text, Text)
setupParseEnv = do
  s <- T.readFile "programs/simple.b"
  f <- T.readFile "programs/fib.b"
  l <- T.readFile "programs/lorem.b"
  pure (s, f, l)

setupInterpEnv :: IO FaunBrick
setupInterpEnv = parseFile "programs/lorem.b"

interpretPure :: FaunBrick -> LT.Text
interpretPure = either err Pure.faunOut . Pure.interpret faun
  where
    faun  = Pure.mkDefaultFaun ""
    err e = error $ "Interpreter error: " <> show e
