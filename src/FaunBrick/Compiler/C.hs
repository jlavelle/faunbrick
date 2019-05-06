{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.C where

import Data.Text (Text)
import TextShow (showt)
import qualified Data.Text.Lazy as LT

import FaunBrick.AST (Program)
import FaunBrick.Common.Types (EofMode(..))
import FaunBrick.Compiler.Language hiding (compile)
import FaunBrick.Compiler.Language.Imperative
import qualified FaunBrick.Compiler.Language as Language

compile :: EofMode -> Program -> LT.Text
compile m = Language.compile cLang (cOpts m)

compile' :: FilePath -> FilePath -> IO ()
compile' = Language.compileFile cLang (cOpts NoChange)

cOpts :: EofMode -> Options
cOpts m =
  let memorySize = MemSize 100000
      initialOffset = Offset 1000
      outputFunction = OutFunc "putchar"
      inputFunction = InFunc "in"
      eofMode = m
  in Options{..}

cLang :: Language
cLang =
  let top Options{..} =
           "#include <stdint.h>\n"
        <> "#include <stdio.h>\n"
        <> cInFunc inputFunction eofMode
        <> "int main(void) {\n"
        <> "    uint8_t m[" <> showt (getMemSize memorySize) <> "] = {0}" <> ceol
        <> "    uint8_t *p = &m[" <> showt (getOffset initialOffset) <> "]" <> ceol
      bottom = "    return 1" <> ceol <> "}"
      indentBody = True
      encoder = cEncoder
  in Language{..}

cEncoder :: Encoder
cEncoder = imperativeEncoder ie
  where
    ie = let iMemAccess = memAccess
             iLoopBegin = "while(*p != 0) {\n"
             iIfBegin   = "if(*p != 0) {\n"
             iBlockEnd  = "}\n"
             iLineEnd   = ceol
             iIndent    = 4
         in ImperativeEncoder{..}

memAccess :: Int -> Text
memAccess n = "p[" <> showt n <> "]"

cInFunc :: InFunc -> EofMode -> Text
cInFunc (InFunc f) m =
     "static uint8_t " <> f <> "(uint8_t val) {\n"
  <> "    int temp = getchar()" <> ceol
  <> "    return (uint8_t)(temp != EOF ? temp : " <> eofVal <> ")" <> ceol
  <> "}\n"
  where
    eofVal = case m of
      NoChange -> "val"
      Zero     -> "0"
      MinusOne -> "-1"
