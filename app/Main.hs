module Main where

import Control.Monad (void)

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as B
import Data.Time (getCurrentTime, diffUTCTime)

import FaunBrick.Interpreter (interpretIO', interpretPure')
import FaunBrick.Parser (parseFile)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Interpreter.Types (TextHandle(..))

runFile :: FilePath -> IO ()
runFile p = void $ optimize <$> parseFile p >>= interpretIO'

runFilePure :: FilePath -> IO ()
runFilePure p = void $ interpretPure' . optimize <$> parseFile p >>= f
  where
    f (Right (_, t)) = T.putStrLn $ B.toLazyText $ textHandleOut t
    f (Left e) = error $ "runFilePure: Interpreter error " <> show e

main :: IO ()
main = do
  putStrLn "Mandelbrot - IO"
  t <- getCurrentTime
  putStrLn $ "Started at " <> show t
  runFile "programs/mandel.b"
  t' <- getCurrentTime
  putStrLn $ "Runtime: " <> show (diffUTCTime t' t)
  putStrLn "99 Bottles - Pure"
  t'' <- getCurrentTime
  putStrLn $ "Started at " <> show t''
  runFilePure "programs/bottles.b"
  t''' <- getCurrentTime
  putStrLn $ "Runtime: " <> show (diffUTCTime t''' t'')
