{-# LANGUAGE RecordWildCards #-}

module FaunBrick.AST.TH where

import Language.Haskell.TH.Quote
import qualified Data.Text.Lazy as T

import FaunBrick.Parser (parseFaunBrick)

fb :: QuasiQuoter
fb = QuasiQuoter{..}
  where
    quoteExp s =
      case parseFaunBrick (T.pack s) of
        Left e  -> fail e
        Right p -> dataToExpQ (const Nothing) p
    quotePat s =
      case parseFaunBrick (T.pack s) of
        Left e  -> fail e
        Right p -> dataToPatQ (const Nothing) p
    quoteType = undefined
    quoteDec  = undefined
