module FaunBrick.Parser (parseFaunBrick, parseFile) where

import Data.Attoparsec.Text
import Data.Functor (($>), (<&>))
import Data.Foldable (asum)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import FaunBrick.AST (Brick(..))

parseFile :: FilePath -> IO [Brick]
parseFile p = either err id . parseFaunBrick <$> T.readFile p
  where
    err e = error $ "Parse error: " <> e

parseFaunBrick :: Text -> Either String [Brick]
parseFaunBrick = parseOnly bricks . sanitize

sanitize :: Text -> Text
sanitize = T.filter (`elem` ['>', '<', '+', '-', '.', ',', '[', ']'])

bricks :: Parser [Brick]
bricks = many1 brick

brick :: Parser Brick
brick = asum
  [ forward
  , backward
  , add
  , sub
  , put
  , get
  , loop
  ]

forward :: Parser Brick
forward = char '>' $> Forward

backward :: Parser Brick
backward = char '<' $> Backward

add :: Parser Brick
add = char '+' $> Add

sub :: Parser Brick
sub = char '-' $> Sub

put :: Parser Brick
put = char '.' $> Put

get :: Parser Brick
get = char ',' $> Get

loop :: Parser Brick
loop = char '[' *> manyTill brick (char ']') <&> Loop

