{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.JS where

import Data.Text (Text)
import TextShow (showt)
import qualified Data.Text.Lazy as LT

import FaunBrick.AST (Program)
import FaunBrick.Common.Types (EofMode(..))
import FaunBrick.Compiler.Language hiding (compile)
import FaunBrick.Compiler.Language.Imperative
import qualified FaunBrick.Compiler.Language as Language

compile :: EofMode -> Program -> LT.Text
compile m = Language.compile jsLang (jsOpts m)

compile' :: FilePath -> FilePath -> IO ()
compile' = Language.compileFile jsLang (jsOpts NoChange)

jsOpts :: EofMode -> Options
jsOpts m =
  let memorySize = MemSize 100000
      initialOffset = Offset 1000
      outputFunction = OutFunc "out"
      inputFunction = InFunc "in"
      eofMode = m
  in Options{..}

-- TODO: Actual in function
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
             iIfBegin   = "if(m[p] !== 0) {\n"
             iBlockEnd  = "}\n"
             iLineEnd   = ceol
             iIndent    = 2
         in ImperativeEncoder{..}

-- e.g. memAccess 1 = "m[p + 1]"
memAccess :: Int -> Text
memAccess n | n > 0 = "m[p + " <> showt n <> "]"
            | n < 0 = "m[p - " <> showt (abs n) <> "]"
            | otherwise = "m[p]"
