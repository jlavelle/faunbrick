module Main where

import Criterion.Main
import qualified Data.Text.IO as T
import Data.Text (Text)

import FaunBrick.Parser (parseFaunBrick)

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(simple, fib, lorem) -> bgroup "Parser"
    [ bench "parse simple.b" $ whnf parseFaunBrick simple
    , bench "parse fib.b"    $ whnf parseFaunBrick fib
    , bench "parse lorem.b"  $ whnf parseFaunBrick lorem
    ]
  ]

setupEnv :: IO (Text, Text, Text)
setupEnv = do
  s <- T.readFile "programs/simple.b"
  f <- T.readFile "programs/fib.b"
  l <- T.readFile "programs/lorem.b"
  pure (s, f, l)
