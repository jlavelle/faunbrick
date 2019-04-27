{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Interpreter.IO (
  interpret, runFile, Faun(..), defaultFaun
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Primitive.Mutable (IOVector)
import Data.IORef (IORef, readIORef, modifyIORef, newIORef)
import Data.Word (Word8)
import GHC.IO.Handle (Handle, hPutChar, hGetChar)
import System.IO (stdin, stdout)
import Data.ByteString.Internal (c2w, w2c)
import Control.Monad (void)
import Data.Functor (($>))

import FaunBrick.MonadFaun (MonadFaun(..))
import qualified FaunBrick.Interpret as Interpret
import FaunBrick.AST (Brick)
import FaunBrick.Parser (parseFile)

runFile :: FilePath -> IO ()
runFile p = do
  f <- defaultFaun
  b <- parseFile p
  void $ interpret f b

interpret :: Foldable f => Faun -> f Brick -> IO Faun
interpret f = runInterpretIO . Interpret.interpret f

newtype InterpretIO a = InterpretIO { runInterpretIO :: IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

data Faun = Faun
  { faunByteArray :: !(IOVector Word8)
  , faunPointer   :: !(IORef Int)
  , faunInHandle  :: !Handle
  , faunOutHandle :: !Handle
  }

instance MonadFaun Faun InterpretIO where
  type Pointer Faun = Int
  type Cell Faun = Word8

  readCurrentCell (Faun v i _ _) = liftIO $ readIORef i >>= MV.read v

  modifyPointer e@(Faun _ i _ _) f = liftIO $ modifyIORef i f $> e

  writeCurrentCell e@(Faun v i _ _) c = liftIO $ do
    idx <- readIORef i
    MV.write v idx c
    pure e

  input e@(Faun v i ih _) = liftIO $ do
    idx <- readIORef i
    c <- c2w <$> hGetChar ih
    MV.write v idx c
    pure e

  output e@(Faun v i _ oh) = liftIO $ do
    idx <- readIORef i
    c <- w2c <$> MV.read v idx
    hPutChar oh c
    pure e

defaultFaun :: IO Faun
defaultFaun = do
  faunByteArray <- MV.replicate 30000 0
  faunPointer   <- newIORef 0
  let faunInHandle  = stdin
      faunOutHandle = stdout
  pure $ Faun {..}
