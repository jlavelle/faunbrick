module Main where

import Criterion.Main
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Text.Lazy (Text)
import Data.Word (Word8)
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy.IO as LT

import FaunBrick.AST (Program)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Common.Types (EofMode(..))
import FaunBrick.Interpreter (interpretPure', interpret)
import FaunBrick.Interpreter.Types (TextHandle(..), Error)
import FaunBrick.Interpreter.Util (defaultTextHandle, defaultMVecMem, defaultIntMapMem)
import FaunBrick.Parser (parseFaunBrick, parseFile')

main :: IO ()
main = defaultMain
  [ env setupParseEnv $ \hanoi -> bgroup "Parser"
    [ bench "parse hanoi.b" $ whnf parseFaunBrick hanoi
    ]
  , env setupInterpEnv $ \ ~(lorem, loremO, bottles, bottlesO) -> bgroup "Interpreters"
    [ bgroup "Pure"
      [ bench "lorem.b"  $ nf interpretPureOut lorem
      , bench "lorem.b with optimizations -- IntMap 1" $ nf interpretPureIntMapOut loremO
      , bench "lorem.b with optimizations" $ nf interpretPureOut loremO
      , bench "lorem.b with optimizations -- IntMap" $ nf interpretPureIntMapOut loremO
      , bench "bottles.b" $ nf interpretPureOut bottles
      , bench "bottles.b with optimizations -- IntMap 1" $ nf interpretPureOut bottlesO
      , bench "bottles.b with optimizations" $ nf interpretPureOut bottlesO
      , bench "bottles.b with optimizations -- IntMap 2" $ nf interpretPureIntMapOut bottlesO
      ]
    , bgroup "IO"
      [ bench "lorem.b" $ nfAppIO interpretIO'' lorem
      , bench "lorem.b with optimizations" $ nfAppIO interpretIO'' loremO
      , bench "bottles.b" $ nfAppIO interpretIO'' bottles
      , bench "bottles.b with optimizations" $ nfAppIO interpretIO'' bottlesO
      ]
    ]
    , env setupOptimizerEnv $ \hanoi -> bgroup "Optimization"
      [ bench "Optimize hanoi.b" $ nf optimize hanoi
      ]
  ]

setupParseEnv :: IO Text
setupParseEnv = LT.readFile "programs/hanoi.b"

setupInterpEnv :: IO (Program, Program, Program, Program)
setupInterpEnv = do
  l <- parseFile' "programs/lorem.b"
  b <- parseFile' "programs/bottles.b"
  pure (l, optimize l, b, optimize b)

setupOptimizerEnv :: IO Program
setupOptimizerEnv = parseFile' "programs/hanoi.b"

interpretPureOut :: Program -> ByteString
interpretPureOut = fromE . interpretPure' @Word8

interpretPureIntMapOut :: Program -> ByteString
interpretPureIntMapOut = fromE @Error
                       . interpret NoChange (defaultIntMapMem @Word8) defaultTextHandle

interpretIO'' :: Program -> IO ByteString
interpretIO'' p =
      defaultMVecMem @Word8
  >>= \m -> interpret NoChange m defaultTextHandle p <&> toLBS . snd

fromE :: Show a => Either a (b, TextHandle c) -> ByteString
fromE (Left e) = error $ "Benchmark: Interpretation error " <> show e
fromE (Right (_, t)) = toLBS t

toLBS :: TextHandle a -> ByteString
toLBS = BS.toLazyByteString . textHandleOut
