module Main where

import FaunBrick.Interpreter.IO (runFile)

main :: IO ()
main = runFile "programs/mandel.b"