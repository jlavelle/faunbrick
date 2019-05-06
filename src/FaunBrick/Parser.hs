module FaunBrick.Parser (parseFaunBrick, parseFile, parseFile') where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text.Lazy
import Data.Foldable (asum, fold)
import Data.Functor (($>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import FaunBrick.AST (FaunBrick(..), Instruction(..), Program)
import FaunBrick.AST.Util (single)

parseFile :: FilePath -> IO (Either String Program)
parseFile p = parseFaunBrick <$> LT.readFile p

parseFile' :: FilePath -> IO Program
parseFile' = fmap (either err id) . parseFile
  where
    err e = error $ "Parse error: " <> e

parseFaunBrick :: Text -> Either String Program
parseFaunBrick = toEither . parse (bricks <|> pure Halt) . sanitize
  where
    toEither (Done _ r)   = Right r
    toEither (Fail _ _ s) = Left s

sanitize :: Text -> Text
sanitize = LT.filter (`elem` ['>', '<', '+', '-', '.', ',', '[', ']'])

bricks :: Parser (FaunBrick Instruction)
bricks = fold <$> many1 brick

brick :: Parser (FaunBrick Instruction)
brick = (single <$> instruction) <|> loop

loop :: Parser (FaunBrick Instruction)
loop = l <$> (char '[' *> bricks <* char ']')
  where
    l i = Loop i Halt

instruction :: Parser Instruction
instruction = asum
  [ forward
  , backward
  , add
  , sub
  , put
  , get
  ]

forward :: Parser Instruction
forward = char '>' $> Jump 1

backward :: Parser Instruction
backward = char '<' $> Jump (-1)

add :: Parser Instruction
add = char '+' $> Update 0 1

sub :: Parser Instruction
sub = char '-' $> Update 0 (-1)

put :: Parser Instruction
put = char '.' $> Output 0

get :: Parser Instruction
get = char ',' $> Input 0
