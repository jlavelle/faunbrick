module FaunBrick.Parser (parseFaunBrick, parseFile) where

import Data.Attoparsec.Text.Lazy
import Data.Functor (($>), (<&>))
import Data.Foldable (asum)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Text.Lazy (Text)

import FaunBrick.AST (Brick(..), FaunBrick)

parseFile :: FilePath -> IO FaunBrick
parseFile p = either err id . parseFaunBrick <$> LT.readFile p
  where
    err e = error $ "Parse error: " <> e

parseFaunBrick :: Text -> Either String FaunBrick
parseFaunBrick = toEither . parse bricks . sanitize
  where
    toEither (Done _ r)   = Right r
    toEither (Fail _ _ s) = Left s

sanitize :: Text -> Text
sanitize = LT.filter (`elem` ['>', '<', '+', '-', '.', ',', '[', ']'])

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

