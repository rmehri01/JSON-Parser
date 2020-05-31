module Parser where

import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.Parsec.Language           ( haskellDef )

data JValue =
    JNull
    | JBool Bool
    | JNumber Double
    | JString String
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Show, Eq)

type Parser a = Parsec String () a

jNullParser :: Parser JValue
jNullParser = JNull <$ string "null"

jBoolParser :: Parser JValue
jBoolParser = JBool True <$ string "true" <|> JBool False <$ string "false"

jNumberParser :: Parser JValue
jNumberParser = JNumber <$> number
  where
    number = (char '-' *> (negate <$> naturalOrFloat)) <|> naturalOrFloat
    naturalOrFloat = try float <|> (fromIntegral <$> natural)

jStringParser :: Parser JValue
jStringParser = JString <$> stringLiteral

jArrayParser :: Parser JValue
jArrayParser = JArray <$> brackets (commaSep jValueParser)

jObjectParser :: Parser JValue
jObjectParser = JObject <$> braces (commaSep pairParser)
  where
    pairParser = (,) <$> (stringLiteral <* spaces <* colon) <*> jValueParser

jValueParser :: Parser JValue
jValueParser =
    jNullParser
        <|> jBoolParser
        <|> jNumberParser
        <|> jStringParser
        <|> jArrayParser
        <|> jObjectParser

------------------

lexer = P.makeTokenParser haskellDef
float = P.float lexer
natural = P.natural lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer
brackets = P.brackets lexer
commaSep = P.commaSep lexer
braces = P.braces lexer
colon = P.colon lexer
