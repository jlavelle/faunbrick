{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.Language.Imperative where

import Data.Text (Text)
import TextShow

import FaunBrick.Compiler.Language

data ImperativeEncoder = ImperativeEncoder
  { iMemAccess :: Int -> Text
  , iLoopBegin :: Text
  , iLoopEnd   :: Text
  , iLineEnd   :: Text
  , iIndent    :: Int
  }

imperativeEncoder :: ImperativeEncoder -> Encoder
imperativeEncoder ImperativeEncoder{..} =
  let output (OutFunc f) o = f <> bracket (iMemAccess o)
      input (InFunc f) o = iMemAccess o <> " = "  <> f
      mulUpdate s d n = mulExpr iMemAccess s d n " += "
      mulSet s d n = mulExpr iMemAccess s d n " = "
      update o n = iMemAccess o <> plusEq n
      set o n = iMemAccess o <> " = " <> showt n
      jump n = "p" <> plusEq n
      lineEnd = Just iLineEnd
      loopBegin = iLoopBegin
      loopEnd = iLoopEnd
      indent = iIndent
  in Encoder{..}

mulExpr :: (Int -> Text) -> Int -> Int -> Int -> Text -> Text
mulExpr memAccess s d n op = memAccess d <> op <> memAccess s <> times
  where
    times | n == 1 = mempty
          | otherwise = " * " <> showt n

ceol :: Text
ceol = ";\n"

plusEq :: Int -> Text
plusEq n | n >= 0 = " += " <> showt n
         | otherwise = " -= " <> showt (abs n)

bracket :: Text -> Text
bracket b = "(" <> b <> ")"
