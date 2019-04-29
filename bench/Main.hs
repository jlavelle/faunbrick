module Main where

import Criterion.Main
import qualified Data.Text.Lazy.IO as LT
import Data.Text.Lazy (Text)
import Data.Functor ((<&>))

import FaunBrick.AST (Program)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFaunBrick, parseFile)
import FaunBrick.Interpreter (interpretPure', interpret)
import FaunBrick.Interpreter.Types (TextHandle(..), UnsafeTextHandle(..))
import FaunBrick.Interpreter.Util (defaultTextHandle, defaultMVecMem)

main :: IO ()
main = defaultMain
  [ env setupParseEnv $ \ ~(simple, fib, lorem) -> bgroup "Parser"
    [ bench "parse simple.b" $ whnf parseFaunBrick simple
    , bench "parse fib.b"    $ whnf parseFaunBrick fib
    , bench "parse lorem.b"  $ whnf parseFaunBrick lorem
    ]
  , env setupInterpEnv $ \ ~(lorem, loremO) -> bgroup "Interpreters"
    [ bgroup "Pure"
      [ bench "interpret lorem.b"  $ nf interpretPureOut lorem
      , bench "interpret lorem.b with optimizations" $ nf interpretPureOut loremO
      ]
    , bgroup "IO"
      [ bench "interpret lorem.b"  $ nfAppIO interpretIO'' lorem
      , bench "interpret lorem.b with optimizations" $ nfAppIO interpretIO'' loremO
      ]
    ]
  ]

setupParseEnv :: IO (Text, Text, Text)
setupParseEnv = do
  s <- LT.readFile "programs/simple.b"
  f <- LT.readFile "programs/fib.b"
  l <- LT.readFile "programs/lorem.b"
  pure (s, f, l)

setupInterpEnv :: IO (Program, Program)
setupInterpEnv = do
  l <- parseFile "programs/lorem.b"
  pure (l, optimize l)

interpretPureOut :: Program -> Text
interpretPureOut p = case interpretPure' p of
  Left e -> error $ "Benchmark: Interpretation error " <> show e
  Right (_, t) -> textHandleOut t

interpretIO'' :: Program -> IO Text
interpretIO'' p = defaultMVecMem >>= \m -> interpret m unsafeHandle p <&> f . snd
  where
    f (UnsafeTextHandle t) = textHandleOut t
    unsafeHandle = UnsafeTextHandle defaultTextHandle
