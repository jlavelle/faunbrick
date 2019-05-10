module Main where

import Criterion.Main
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LT

import FaunBrick.AST (Program)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFaunBrick, parseFile')

-- TODO: Restore interpreter benchmarks
main :: IO ()
main = defaultMain
  [ env setupParseEnv $ \hanoi -> bgroup "Parser"
    [ bench "parse hanoi.b" $ whnf parseFaunBrick hanoi
    ]
  , env setupOptimizerEnv $ \hanoi -> bgroup "Optimization"
    [ bench "Optimize hanoi.b" $ nf optimize hanoi
    ]
  ]

setupParseEnv :: IO Text
setupParseEnv = LT.readFile "programs/hanoi.b"

setupOptimizerEnv :: IO Program
setupOptimizerEnv = parseFile' "programs/hanoi.b"
