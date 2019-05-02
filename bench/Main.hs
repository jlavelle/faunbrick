module Main where

import Criterion.Main
import qualified Data.Text.Lazy.IO as LT
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as B
import Data.Functor ((<&>))

import FaunBrick.AST (Program)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFaunBrick, parseFile)
import FaunBrick.Interpreter (interpretPure', interpret)
import FaunBrick.Interpreter.Types (TextHandle(..), UnsafeTextHandle(..))
import FaunBrick.Interpreter.Util (defaultTextHandle, defaultMVecMem, defaultTape)

main :: IO ()
main = defaultMain
  [ env setupParseEnv $ \ ~(simple, fib, lorem) -> bgroup "Parser"
    [ bench "parse simple.b" $ whnf parseFaunBrick simple
    , bench "parse fib.b"    $ whnf parseFaunBrick fib
    , bench "parse lorem.b"  $ whnf parseFaunBrick lorem
    ]
  , env setupInterpEnv $ \ ~(lorem, loremO, bottles, bottlesO) -> bgroup "Interpreters"
    [ bgroup "Pure"
      [ bench "interpret lorem.b"  $ nf interpretPureOut lorem
      , bench "interpret lorem.b with optimizations" $ nf interpretPureOut loremO
      , bench "interpret bottles.b" $ nf interpretPureOut bottles
      , bench "interpret bottles.b with optimizations" $ nf interpretPureOut bottlesO
      , bench "bottles.b with optimizations -- Tape" $ nf interpretPureTapeOut bottlesO
      ]
    , bgroup "IO"
      [ bench "interpret lorem.b"  $ nfAppIO interpretIO'' lorem
      , bench "interpret lorem.b with optimizations" $ nfAppIO interpretIO'' loremO
      , bench "interpret bottles.b" $ nfAppIO interpretIO'' bottles
      , bench "interpret bottles.b with optimizations" $ nfAppIO interpretIO'' bottlesO
      ]
    ]
    , env setupOptimizerEnv $ \mandel -> bgroup "Optimization"
      [ bench "Optimize mandel.b" $ nf optimize mandel
      ]
  ]

setupParseEnv :: IO (Text, Text, Text)
setupParseEnv = do
  s <- LT.readFile "programs/simple.b"
  f <- LT.readFile "programs/fib.b"
  l <- LT.readFile "programs/lorem.b"
  pure (s, f, l)

setupInterpEnv :: IO (Program, Program, Program, Program)
setupInterpEnv = do
  l <- parseFile "programs/lorem.b"
  b <- parseFile "programs/bottles.b"
  pure (l, optimize l, b, optimize b)

setupOptimizerEnv :: IO Program
setupOptimizerEnv = parseFile "programs/mandel.b"

interpretPureOut :: Program -> Text
interpretPureOut p = case interpretPure' p of
  Left e -> error $ "Benchmark: Interpretation error " <> show e
  Right (_, t) -> B.toLazyText $ textHandleOut t

interpretPureTapeOut :: Program -> Text
interpretPureTapeOut p = case interpret defaultTape defaultTextHandle p of
  Left e -> error $ "Benchmark: Interpretation error " <> show e
  Right (_, t) -> B.toLazyText $ textHandleOut t

interpretIO'' :: Program -> IO Text
interpretIO'' p = defaultMVecMem >>= \m -> interpret m unsafeHandle p <&> f . snd
  where
    f (UnsafeTextHandle t) = B.toLazyText $ textHandleOut t
    unsafeHandle = UnsafeTextHandle defaultTextHandle
