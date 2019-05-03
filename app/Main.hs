module Main where

import Options.Applicative
import Control.Monad (void)

import FaunBrick.Interpreter (interpretIO')
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.Parser (parseFile)
import FaunBrick.Compiler.JS (compileJS)
import FaunBrick.Compiler.C (compileC)

data Language = JS | C

data Mode
  = Interpret FilePath
  | Compile Language FilePath FilePath

subcommand :: String -> String -> Parser a -> Parser a
subcommand n d p = hsubparser
  (  command n (info p (fullDesc <> progDesc d))
  <> metavar n
  )
mode :: Parser Mode
mode = subcommand "interpret" "Interpret a file" interpretOpts
   <|> subcommand "compile" "Compile a file to another language" compileOpts
  where
    interpretOpts = Interpret <$> argument str (metavar "FILE" <> help "File to interpret")
    compileOpts = Compile
              <$> parseLanguage
              <*> argument str (metavar "SOURCE" <> help "Source file")
              <*> argument str (metavar "DESTINATION" <> help "Destination file")

parseLanguage :: Parser Language
parseLanguage = subcommand "js" "Compile a file to JavaScript" (pure JS)
            <|> subcommand "c" "Compile a file to C" (pure C)

handler :: Mode -> IO ()
handler (Interpret p) = void $ optimize <$> parseFile p >>= interpretIO'
handler (Compile l s d) = case l of
  JS -> compileJS s d
  C  -> compileC s d

main :: IO ()
main = execParser p >>= handler
  where
    p = info mode
      (  fullDesc
      <> progDesc "Interpret or compile a Brainfuck source file."
      )

