module FaunBrick.Parser (parseFaunBrick, parseFile) where

import Data.Attoparsec.Text.Lazy
import Data.Functor (($>))
import Data.Foldable (asum, fold)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Text.Lazy (Text)
import Control.Applicative ((<|>))

import FaunBrick.AST (FaunBrick(..), Instruction(..))
import FaunBrick.AST.Util (single)

parseFile :: FilePath -> IO (FaunBrick Instruction)
parseFile p = either err id . parseFaunBrick <$> LT.readFile p
  where
    err e = error $ "Parse error: " <> e

parseFaunBrick :: Text -> Either String (FaunBrick Instruction)
parseFaunBrick = toEither . parse bricks . sanitize
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
put = char '.' $> Put 0

get :: Parser Instruction
get = char ',' $> Get 0
