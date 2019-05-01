{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.JS where

import Data.Text (Text)
import TextShow

import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Compiler.Language
import FaunBrick.Compiler.Language.Imperative

compileJS :: FilePath -> FilePath -> IO ()
compileJS = compileFile jsLang jsOpts

jsOpts :: Options
jsOpts =
  let memorySize = MemSize 100000
      initialOffset = Offset 1000
      outputFunction = OutFunc "out"
      inputFunction = InFunc "in"
      optimization = optimize
  in Options{..}

jsLang :: Language
jsLang =
  let top Options{..} =
           "let output = ''" <> ceol
        <> "const out = b => {\n"
        <> "  output = output + String.fromCharCode(b)" <> ceol
        <> "}\n"
        <> "const start = Date.now()" <> ceol
        <> "let p = " <> showt (getOffset initialOffset) <> ceol
        <> "const m = new Uint8Array(" <> showt (getMemSize memorySize) <> ")" <> ceol
      bottom =
           "const end = Date.now()" <> ceol
        <> "console.log(output)" <> ceol
        <> "console.log(end - start)" <> ceol
      indentBody = False
      encoder = jsEncoder
  in Language{..}

jsEncoder :: Encoder
jsEncoder = imperativeEncoder ie
  where
    ie = let iMemAccess = memAccess
             iLoopBegin = "while(m[p] !== 0) {\n"
             iLoopEnd   = "}\n"
             iLineEnd   = ceol
             iIndent    = 2
         in ImperativeEncoder{..}

-- e.g. memAccess 1 = "m[p + 1]"
memAccess :: Int -> Text
memAccess n | n > 0 = "m[p + " <> showt n <> "]"
            | n < 0 = "m[p - " <> showt (abs n) <> "]"
            | otherwise = "m[p]"
