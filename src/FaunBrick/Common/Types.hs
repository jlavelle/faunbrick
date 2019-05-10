module FaunBrick.Common.Types where

data EofMode
  = NoChange
  | MinusOne
  | Zero
  deriving Show

data BitWidth
  = Width8
  | Width16
  | Width32
  | Width64
  deriving Show

data Error = OutOfBounds deriving Show
