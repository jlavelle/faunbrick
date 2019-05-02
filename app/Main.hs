module Main where

import Control.Monad (void)

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as B
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Monoid (Endo(..))
import Data.Foldable (fold, traverse_)
import Data.List (inits)

import FaunBrick.Interpreter (interpretIO', interpretPure')
import FaunBrick.Parser (parseFile)
import FaunBrick.AST.Optimize
import FaunBrick.AST (Program)
import FaunBrick.Interpreter.Types (TextHandle(..))

runFile :: FilePath -> IO ()
runFile p = void $ optimize <$> parseFile p >>= flip timed p

runFilePure :: FilePath -> IO ()
runFilePure p = void $ interpretPure' . optimize <$> parseFile p >>= f
  where
    f (Right (_, t)) = T.putStrLn $ B.toLazyText $ textHandleOut t
    f (Left e) = error $ "runFilePure: Interpreter error " <> show e

subOpts :: (forall a. [a] -> [[a]]) -> [(Int, Optimization)]
subOpts f = zip [0..] opts
  where
    opts = fmap (eqFix . appEndo . fold) $ f $ fmap Endo
      [ loopsToMul
      , contract
      , fuse
      , elimClears
      , offsets
      , uninterpose
      , loopsToIfs
      , dedupMulSet
      ]

progOpts :: (forall a. [a] -> [[a]]) -> Program -> [(Int, Program)]
progOpts f p = fmap (fmap ($ p)) $ subOpts f

main :: IO ()
main = do
  p <- parseFile "programs/mandel.b"
  traverse_ go $ progOpts inits p
  where
    go (i, p) = timed p $ "Mandelbrot with opt " <> show i


timed :: Program -> String -> IO ()
timed p s = do
  putStrLn s
  t <- getCurrentTime
  putStrLn $ "Statred at " <> show t
  interpretIO' p
  t' <- getCurrentTime
  putStrLn $ "Runtime: " <> show (diffUTCTime t' t)
