module FaunBrick.Interpreter.Pure where

import Data.Functor ((<&>))
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy as LT

import FaunBrick.Common.Types (Error(..))
import FaunBrick.Interpreter.Types

data TextHandle a = TextHandle
  { textHandleIn  :: LT.Text
  , textHandleOut :: BS.Builder
  }

instance Packable a => PureHandle (TextHandle a) a where
  outputP (TextHandle i o) a = TextHandle i $ o <> pack a
  inputP h@(TextHandle i o) = case LT.uncons i of
    Just (a, r) -> (Just $ fromChar a, TextHandle r o)
    Nothing -> (Nothing, h)

data Tape a = Tape [a] a [a] deriving Show

-- TODO: Look at more efficient ways to handle Tape movement
instance PureMemory (Tape (Int, a)) a where
  readCellP t i = snd . readFocus <$> diffMove t (+ i)
  writeCellP t i a = tapeMod t i (const a)
  modifyCellP = tapeMod

tapeMod :: Tape (Int, a) -> Int -> (a -> a) -> Either Error (Tape (Int, a))
tapeMod t o f = diffMove t (+ o) <&> modifyFocus (\(n, x) -> (n, f x))

readFocus :: Tape a -> a
readFocus (Tape _ a _) = a

leftN :: Integral n => n -> Tape a -> Either Error (Tape a)
leftN 0 t = Right t
leftN _ (Tape [] _ _) = Left OutOfBounds
leftN n (Tape (l:ls) f rs) = leftN (n - 1) (Tape ls l (f:rs))

rightN :: Integral n => n -> Tape a -> Either Error (Tape a)
rightN 0 t = Right t
rightN _ (Tape _ _ []) = Left OutOfBounds
rightN n (Tape ls f (r:rs)) = rightN (n - 1) (Tape (f:ls) r rs)

modifyFocus :: (a -> a) -> Tape a -> Tape a
modifyFocus f (Tape ls a rs) = Tape ls (f a) rs

diffMove :: Tape (Int, a) -> (Int -> Int) -> Either Error (Tape (Int, a))
diffMove t f =
  let x = fst $ readFocus t
      y = f x
  in case compare y x of
    GT -> rightN (y - x) t
    LT -> leftN (x - y) t
    _  -> Right t
