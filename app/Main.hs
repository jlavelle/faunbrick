module Main where

import Control.Monad (void)

import FaunBrick.Interpreter (interpretIO')
import FaunBrick.Parser (parseFile)
import FaunBrick.AST.Optimize (optimize)

runFile :: FilePath -> IO ()
runFile p = void $ optimize <$> parseFile p >>= interpretIO'

main :: IO ()
main = runFile "programs/mandel.b"
