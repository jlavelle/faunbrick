module FaunBrick.Interpreter.IO where

import System.IO.Error (isEOFError, catchIOError)
import qualified Data.ByteString.Builder as BS
import qualified GHC.IO.Handle as GHC

import FaunBrick.Interpreter.Types (Packable(..), Handle(..))

data IOHandle a = IOHandle
  { ioHandleIn  :: GHC.Handle
  , ioHandleOut :: GHC.Handle
  }

instance Packable a => Handle (IOHandle a) IO a where
  input (IOHandle i _) = catchIOError (Just . fromChar <$> GHC.hGetChar i)
                       $ \e -> if isEOFError e then pure Nothing else ioError e
  {-# INLINE input #-}
  output (IOHandle _ o) a = BS.hPutBuilder o (pack a)
  {-# INLINE output #-}
