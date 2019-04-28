module Main where

import Criterion.Main
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word (Word8)

import FaunBrick.AST (FaunBrick)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFaunBrick, parseFile)
import qualified FaunBrick.Interpreter.Pure as Pure
import qualified FaunBrick.Interpreter.IO as IO

main :: IO ()
main = defaultMain
  [ -- env setupParseEnv $ \ ~(simple, fib, lorem) -> bgroup "Parser"
    -- [ bench "parse simple.b" $ whnf parseFaunBrick simple
    -- , bench "parse fib.b"    $ whnf parseFaunBrick fib
    -- , bench "parse lorem.b"  $ whnf parseFaunBrick lorem
    -- ]
    env setupInterpEnv $ \ ~(lorem, loremO) -> bgroup "Interpreters"
    [ bgroup "Pure"
      [ bench "interpret lorem.b"  $ nf interpretPure lorem
      , bench "interpret lorem.b with optimizations" $ nf interpretPure loremO
      -- , bench "interpret mandel.b" $ nf interpretPure mandel
      ]
    --, bgroup "IO"
    --  [ bench "interpret lorem.b"  $ whnfAppIO interpretIO lorem
      -- , bench "interpret mandel.b" $ whnfAppIO interpretIO mandel
     -- ]
    ]
  ]

setupParseEnv :: IO (Text, Text, Text)
setupParseEnv = do
  s <- T.readFile "programs/simple.b"
  f <- T.readFile "programs/fib.b"
  l <- T.readFile "programs/lorem.b"
  pure (s, f, l)

setupInterpEnv :: IO (FaunBrick, FaunBrick)
setupInterpEnv = do
  l <- parseFile "programs/lorem.b"
  m <- parseFile "programs/mandel.b"
  pure (l, optimize l)

interpretPure :: FaunBrick -> LT.Text
interpretPure = either err Pure.faunOut . (Pure.interpret @Word8) faun
  where
    faun  = Pure.mkDefaultFaun ""
    err e = error $ "Interpreter error: " <> show e

interpretIO :: FaunBrick -> IO IO.Faun
interpretIO b = quietFaun >>= \f -> IO.interpret f b

quietFaun :: IO IO.Faun
quietFaun = (\f -> f { IO.faunQuiet = True }) <$> IO.defaultFaun
