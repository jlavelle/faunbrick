{-# LANGUAGE RecordWildCards #-}

module FaunBrick.Compiler.C where

import Data.Text (Text)
import TextShow

import FaunBrick.Compiler.Language
import FaunBrick.Compiler.Language.Imperative
import FaunBrick.AST.Optimize (optimize)

compileC :: FilePath -> FilePath -> IO ()
compileC = compileFile cLang cOpts

cOpts :: Options
cOpts =
  let memorySize = MemSize 100000
      initialOffset = Offset 1000
      outputFunction = OutFunc "putchar"
      inputFunction = InFunc "in"
      optimization = optimize
  in Options{..}

cLang :: Language
cLang =
  let top Options{..} =
           "#include <stdint.h>\n"
        <> "#include <stdio.h>\n"
        <> "static uint8_t in() {\n"
        <> "    int temp = getchar()" <> ceol
        <> "    return (uint8_t)(temp != EOF ? temp : 0)" <> ceol
        <> "}\n\n"
        <> "int main(void) {\n"
        <> "    uint8_t m[" <> showt (getMemSize memorySize) <> "] = {0}" <> ceol
        <> "    uint8_t *p = &m[" <> showt (getOffset initialOffset) <> "]" <> ceol
      bottom = "    return 1;" <> ceol <> "}"
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
