{-# OPTIONS_HADDOCK prune #-}
-- | This module is for setting up the built in token functions with the default haskell language lexer
-- More information at: https://hackage.haskell.org/package/parsec/docs/Text-ParserCombinators-Parsec-Token.html
module Tokens where

import qualified Text.Parsec.Token             as P
import           Text.Parsec.Language           ( haskellDef )

lexer = P.makeTokenParser haskellDef
float = P.float lexer
natural = P.natural lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer
brackets = P.brackets lexer
commaSep = P.commaSep lexer
braces = P.braces lexer
colon = P.colon lexer