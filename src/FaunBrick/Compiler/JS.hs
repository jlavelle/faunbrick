{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.JS where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Foldable (fold)

import FaunBrick.AST (Brick(..), FaunBrick)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFile)

compile :: Options -> FilePath -> FilePath -> IO ()
compile o from to = do
  src <- optimize <$> parseFile from
  LT.writeFile to $ B.toLazyText $ encodeFaunBrick o src

data Options = Options
  { memorySize    :: Int
  , initialOffset :: Int
  , outFunc       :: Text
  , inFunc        :: Text
  }

defaultOptions :: Options
defaultOptions = Options
  { memorySize    = 30000
  , initialOffset = 0
  , outFunc       = "out"
  , inFunc        = "in"
  }

top :: Options -> Builder
top Options{..} = "let p = " <> B.decimal initialOffset <> eol
               <> "const m = new Uint8Array(" <> B.decimal memorySize <> ")" <> eol

encodeFaunBrick :: Options -> FaunBrick -> Builder
encodeFaunBrick Options{..} x = top Options{..} <> encode 0 x
  where
    encode n = foldMap (indent n . go)
      where
        go Forward    = "p++" <> eol
        go Backward   = "p--" <> eol
        go Add        = "m[p] += 1" <> eol
        go Sub        = "m[p] -= 1" <> eol
        go Put        = B.fromText outFunc <> "(m[p])" <> eol
        go Get        = B.fromText inFunc <> "(m, p)" <> eol
        go (Update i) = "m[p] += " <> B.decimal i <> eol
        go (Jump i)   = "p += " <> B.decimal i <> eol
        go Clear      = "m[p] = 0" <> eol
        go (Loop bs)  = "while(m[p] !== 0) {\n" <> encode (n + 1) bs <> indent n "}\n"

eol :: Builder
eol = ";\n"

indent :: Int -> Builder -> Builder
indent n b = fold (replicate (n * 2) " ") <> b
