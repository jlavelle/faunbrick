{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Options.Applicative as Opts
import Options.Applicative (Parser, ParserInfo)
import Control.Monad (void)
import Control.Applicative (optional, (<|>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import FaunBrick.Common.Types (EofMode(..))
import qualified FaunBrick.Interpreter as Interpreter
import qualified FaunBrick.AST.Optimize as AST
import qualified FaunBrick.Parser as Parser
import qualified FaunBrick.Compiler.JS
import qualified FaunBrick.Compiler.C

data Options = Options
  { mode    :: Mode
  , source  :: Either Text FilePath
  , eofMode :: EofMode
  }

data Mode
  = Interpret
  | Compile CompileOpts

data CompileOpts = CompileOpts
  { language    :: Language
  , destination :: Maybe FilePath
  }

data Language = JS | C

parseOptions :: Parser Options
parseOptions =
      Options
  <$> parseMode
  <*> parseSource
  <*> parseEofMode

parseMode :: Parser Mode
parseMode =
      subcommand
        "interpret"
        "Interpret Brainfuck source"
        (pure Interpret)
  <|> subcommand
        "compile"
        "Compile to Brainfuck to another language"
        (Compile <$> parseCompileOpts)
  where
    parseCompileOpts =
          CompileOpts
      <$> parseLanguage
      <*> optional parseDestination

    parseLanguage =
          subcommand "js" "Compile to JavaScript" (pure JS)
      <|> subcommand "c" "Compile to C" (pure C)

    parseDestination =
      Opts.strOption
      (  Opts.long "to"
      <> Opts.help "Destination file"
      <> Opts.metavar "FILE"
      )

parseSource :: Parser (Either Text FilePath)
parseSource = fmap Right filePath <|> fmap (Left . LT.pack) snippet
  where
    filePath =
      Opts.strArgument
      (  Opts.metavar "SOURCE"
      <> Opts.help "Specify a file as the source"
      )
    snippet =
      Opts.strOption
      (  Opts.long "snippet"
      <> Opts.short 's'
      <> Opts.metavar "SNIPPET"
      <> Opts.help "Use a snippet of code as the source"
      )

parseEofMode :: Parser EofMode
parseEofMode =
      hflag' NoChange "eof-no-change" "Do not change the current cell on input EOF"
  <|> hflag' Zero     "eof-zero"      "Set the current cell to zero on input EOF"
  <|> hflag' MinusOne "eof-minus-one" "Set the current cell to -1 on input EOF"
  <|> pure   NoChange

subcommand :: String -> String -> Parser a -> Parser a
subcommand n d p = Opts.hsubparser
  (  Opts.command n (Opts.info p (Opts.fullDesc <> Opts.progDesc d))
  <> Opts.metavar n
  )

hflag' :: a -> String -> String -> Parser a
hflag' a l h = Opts.flag' a (Opts.long l <> Opts.help h)

parserInfo :: ParserInfo Options
parserInfo =
  Opts.info (Opts.helper <*> parseOptions)
  (  Opts.fullDesc
  <> Opts.progDesc "Interpret or compile Brainfuck code"
  <> Opts.header "faunbrick - An optimizing Brainfuck interpreter & compiler"
  )

handler :: Options -> IO ()
handler Options{..} = do
  src <- AST.optimize <$> sourceOrError source
  case mode of
    Interpret -> handleInterpret eofMode src
    Compile o -> handleCompile o eofMode src
  where
    sourceOrError s =
          either (pure . Parser.parseFaunBrick) Parser.parseFile s
      >>= either die pure

    die e = error $ "Fatal error: " <> show e

    handleInterpret m = void . Interpreter.interpretIO' m

    handleCompile CompileOpts{..} m p =
      let c = compile language m p
      in maybe (LT.putStrLn c) (`LT.writeFile` c) destination

    compile l m p = case l of
      JS -> FaunBrick.Compiler.JS.compile m p
      C  -> FaunBrick.Compiler.C.compile m p

main :: IO ()
main = Opts.execParser parserInfo >>= handler
