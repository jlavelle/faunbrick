module Main where

import Criterion.Main
import qualified Data.Text.Lazy.IO as LT
import Data.Functor ((<&>))
import Data.Text.Lazy (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as BS
import Data.Word (Word8)

import FaunBrick.AST (Program)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFaunBrick, parseFile')
import FaunBrick.Interpreter (interpretPure', interpret)
import FaunBrick.Interpreter.Types (TextHandle(..))
import FaunBrick.Common.Types (EofMode(..))
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
  l <- parseFile' "programs/lorem.b"
  b <- parseFile' "programs/bottles.b"
  pure (l, optimize l, b, optimize b)

setupOptimizerEnv :: IO Program
setupOptimizerEnv = parseFile' "programs/mandel.b"

interpretPureOut :: Program -> ByteString
interpretPureOut = fromE . interpretPure' @Word8

interpretPureTapeOut :: Program -> ByteString
interpretPureTapeOut = fromE . interpret NoChange (defaultTape @Word8) defaultTextHandle

interpretIO'' :: Program -> IO ByteString
interpretIO'' p =
      defaultMVecMem @Word8
  >>= \m -> interpret NoChange m defaultTextHandle p <&> toLBS . snd

fromE :: Show a => Either a (b, TextHandle c) -> ByteString
fromE (Left e) = error $ "Benchmark: Interpretation error " <> show e
fromE (Right (_, t)) = toLBS t

toLBS :: TextHandle a -> ByteString
toLBS = BS.toLazyByteString . textHandleOut
