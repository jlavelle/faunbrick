{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.Language where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as B
import Data.Foldable (fold)

import FaunBrick.AST.Optimize (Optimization)
import FaunBrick.AST (Program, Instruction(..), FaunBrick(..))
import FaunBrick.Parser (parseFile)

newtype MemSize = MemSize { getMemSize :: Int }
newtype Offset = Offset { getOffset :: Int }
newtype InFunc = InFunc { getInFunc :: Text }
newtype OutFunc = OutFunc { getOutFunc :: Text }

data Options = Options
  { memorySize     :: MemSize
  , initialOffset  :: Offset
  , outputFunction :: OutFunc
  , inputFunction  :: InFunc
  , optimization   :: Optimization
  }

data Language = Language
  { top        :: Options -> Text
  , bottom     :: Text
  , encoder    :: Encoder
  , indentBody :: Bool
  }

data Encoder = Encoder
  { output    :: OutFunc -> Int -> Text
  , input     :: InFunc -> Int -> Text
  , update    :: Int -> Int -> Text
  , jump      :: Int -> Text
  , set       :: Int -> Int -> Text
  , mulUpdate :: Int -> Int -> Int -> Text
  , mulSet    :: Int -> Int -> Int -> Text
  , lineEnd   :: Maybe Text
  , loopBegin :: Text
  , loopEnd   :: Text
  , ifBegin   :: Text
  , ifEnd     :: Text
  , indent    :: Int
  }

compileFile :: Language -> Options -> FilePath -> FilePath -> IO ()
compileFile l o from to = do
  src <- parseFile from
  LT.writeFile to $ B.toLazyText $ compile l o src

compile :: Language -> Options -> Program -> Builder
compile l@Language{..} o@Options{..} p =
  B.fromText (top o) <> go optimized bodyIndent <> B.fromText bottom
  where
    optimized = optimization p
    bodyIndent = if indentBody then 1 else 0

    go :: Program -> Int -> Builder
    go Halt _ = mempty
    go (Instr i r) n = indented (indent encoder) n $ encode l o i <> go r n
    go (Loop as r) n = block as r (loopBegin encoder) (loopEnd encoder) n
    go (If as r) n   = block as r (ifBegin encoder) (ifEnd encoder) n

    block as r begin end n =
      let Encoder{..} = encoder
      in indented indent n (B.fromText begin)
         <> go as (n + 1)
         <> indented indent n (B.fromText end)
         <> go r n

encode :: Language -> Options -> Instruction -> Builder
encode Language{..} Options{..} i =
  let Encoder{..} = encoder
      instr = case i of
        Output o -> output outputFunction o
        Input o  -> input inputFunction o
        Update o n -> update o n
        Jump n -> jump n
        Set o n -> set o n
        MulUpdate s d n -> mulUpdate s d n
        MulSet s d n -> mulSet s d n
  in B.fromText $ instr <> maybe mempty id lineEnd

indented :: Int -> Int -> Builder -> Builder
indented level n b = fold (replicate (n * level) " ") <> b
