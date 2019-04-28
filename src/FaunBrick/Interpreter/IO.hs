{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Interpreter.IO (
  interpret, runFile, Faun(..), defaultFaun
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Primitive.Mutable (IOVector)
import Data.Word (Word8)
import GHC.IO.Handle (Handle, hPutChar, hGetChar)
import System.IO (stdin, stdout)
import Data.ByteString.Internal (c2w, w2c)
import Control.Monad (void, unless)

import FaunBrick.MonadFaun (MonadFaun(..))
import qualified FaunBrick.Interpret as Interpret
import FaunBrick.AST (FaunBrick)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFile)

runFile :: FilePath -> IO ()
runFile p = do
  f <- defaultFaun
  b <- optimize <$> parseFile p
  void $ interpret f b

interpret :: Faun -> FaunBrick -> IO Faun
interpret f = runInterpretIO . Interpret.interpret f

newtype InterpretIO a = InterpretIO { runInterpretIO :: IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

data Faun = Faun
  { faunByteArray :: !(IOVector Word8)
  , faunPointer   :: !Int
  , faunInHandle  :: !Handle
  , faunOutHandle :: !Handle
  , faunQuiet     :: !Bool
  }

instance MonadFaun Faun InterpretIO where
  type Pointer Faun = Int
  type Cell Faun = Word8

  readCurrentCell (Faun v i _ _ _) = liftIO $ MV.read v i

  modifyPointer e f = pure $ e { faunPointer = f $ faunPointer e }

  writeCurrentCell e@(Faun v i _ _ _) c = liftIO $ do
    MV.write v i c
    pure e

  input e@(Faun v i ih _ _) = liftIO $ do
    c <- c2w <$> hGetChar ih
    MV.write v i c
    pure e

  output e@(Faun v i _ oh q) = liftIO $ do
    c <- w2c <$> MV.read v i
    unless q $ hPutChar oh c
    pure e

defaultFaun :: IO Faun
defaultFaun = do
  faunByteArray <- MV.replicate 30000 0
  let faunPointer = 0
      faunInHandle  = stdin
      faunOutHandle = stdout
      faunQuiet = False
  pure $ Faun {..}
