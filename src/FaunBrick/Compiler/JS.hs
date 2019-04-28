{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.JS where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Foldable (fold)

import FaunBrick.AST (FaunBrick(..), FaunBrickF(..), Instruction(..), Program)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFile)

compileFile :: Options -> FilePath -> FilePath -> IO ()
compileFile o from to = do
  src <- optimize <$> parseFile from
  LT.writeFile to $ B.toLazyText $ compile o src

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
top Options{..} = "let output = ''" <> eol
               <> "const out = b => {\n"
               <> indent 1 "output = output + String.fromCharCode(b)" <> eol <> "}\n"
               <> "const start = Date.now()" <> eol
               <> "let p = " <> B.decimal initialOffset <> eol
               <> "const m = new Uint8Array(" <> B.decimal memorySize <> ")" <> eol

bottom :: Builder
bottom = "const end = Date.now()" <> eol
      <> "console.log(output)" <> eol
      <> "console.log(end - start)" <> eol

compile :: Options -> Program -> Builder
compile o@Options{..} x = top o <> go x 0 <> bottom
  where
    go :: Program -> Int -> Builder
    go Halt _ = mempty
    go (Instr i r) n = indent n $ encode o i <> go r n
    go (Loop as r) n = wl <> go as (n + 1) <> indent n "}\n" <> go r n
      where
        wl = indent n "while(m[p] !== 0) {\n"

encode :: Options -> Instruction -> Builder
encode Options{..} i = case i of
  Forward    -> "p++" <> eol
  Backward   -> "p--" <> eol
  Add        -> "m[p] += 1" <> eol
  Sub        -> "m[p] -= 1" <> eol
  Put        -> B.fromText outFunc <> "(m[p])" <> eol
  Get        -> B.fromText inFunc <> "(m, p)" <> eol
  (Update n) -> "m[p] += " <> B.decimal n <> eol
  (Jump n)   -> "p += " <> B.decimal n <> eol
  Clear      -> "m[p] = 0" <> eol

eol :: Builder
eol = ";\n"

indent :: Int -> Builder -> Builder
indent n b = fold (replicate (n * 2) " ") <> b
