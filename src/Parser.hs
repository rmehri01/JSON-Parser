module Parser where

import           Text.Parsec
import           Tokens

data JValue =
    JNull
    | JBool Bool
    | JNumber Double
    | JString String
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Show, Eq)

-- | Type alias for Parsec that takes a String as input and outputs 'a'
-- Does not utilize the user state, as given by the ()
type Parser a = Parsec String () a

-- | Only parses the keyword null with no whitespace
--
-- >>> parseTest jNullParser "null"
-- JNull
-- 
-- >>> parseTest jNullParser "foo"
-- parse error at (line 1, column 1):
-- unexpected "f"
-- expecting "null"
jNullParser :: Parser JValue
jNullParser = JNull <$ string "null"

-- | Parses either true or false with no whitespace
--
-- >>> parseTest jBoolParser "true"
-- JBool True
--
-- >>> parseTest jBoolParser "bar"
-- parse error at (line 1, column 1):
-- unexpected "b"
-- expecting "true" or "false"
jBoolParser :: Parser JValue
jBoolParser = JBool True <$ string "true" <|> JBool False <$ string "false"

-- | Parses a double or integer value
-- 
-- >>> parseTest jNumberParser "-23.6"
-- JNumber (-23.6)
-- 
-- >>> parseTest jNumberParser "42"
-- JNumber 42.0
--
-- >>> parseTest jNumberParser "baz"
-- parse error at (line 1, column 1):
-- unexpected "b"
-- expecting "-", float or natural
jNumberParser :: Parser JValue
jNumberParser = JNumber <$> number
 where
  number         = (char '-' *> (negate <$> naturalOrFloat)) <|> naturalOrFloat
  naturalOrFloat = try float <|> (fromIntegral <$> natural)

-- | Parses a string with double quotes around it, returning what is inside
--
-- >>> parseTest jStringParser  "\"hello\""
-- JString "hello"
--
-- This example is missing the double quotes:
-- >>> parseTest jStringParser  "hello"
-- parse error at (line 1, column 1):
-- unexpected "h"
-- expecting literal string
jStringParser :: Parser JValue
jStringParser = JString <$> stringLiteral

-- | Parses any string with double quotes around it, returning what is inside
-- Currently only works for whitespace escaped characters such as '\n'
-- Other escaped characters will remain in their raw form
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill (anyChar <|> space) (char '\"')

-- | Parses an array surrounded by brackets and separated by commas and whitespace
--
-- >>> parseTest jArrayParser  "[1, 2, 3, 4]"
-- JArray [JNumber 1.0,JNumber 2.0,JNumber 3.0,JNumber 4.0]
-- 
-- Since it is a JSON, there can be different types within the array:
-- >>> parseTest jArrayParser  "[1, true, null, \"hello\"]"
-- JArray [JNumber 1.0,JBool True,JNull,JString "hello"]
--
-- >>> parseTest jArrayParser  "1, true, null, \"hello\"]"
-- parse error at (line 1, column 1):
-- unexpected "1"
-- expecting "["
jArrayParser :: Parser JValue
jArrayParser = JArray <$> brackets (commaSep jValueParser)

-- | Parses an object of String and JValue pairs surrounded by braces and separated by commas and whitespace
-- 
-- >>> parseTest jObjectParser "{ \"first\" : 24, \"second\" : null }"
-- JObject [("first",JNumber 24.0),("second",JNull)]
--
-- >>> parseTest jObjectParser " \"first\" : 24, \"second\" : null }"
-- parse error at (line 1, column 1):
-- unexpected " "
-- expecting "{"
jObjectParser :: Parser JValue
jObjectParser = JObject <$> braces (commaSep pairParser)
 where
  pairParser = (,) <$> (stringLiteral <* spaces <* colon) <*> jValueParser

-- | Combines all parsers into a single JSON parser that allows for any amount of whitespace between them
jValueParser :: Parser JValue
jValueParser =
  spaces
    *> (   jNullParser
       <|> jBoolParser
       <|> jNumberParser
       <|> jStringParser
       <|> jArrayParser
       <|> jObjectParser
       )
    <* spaces

-- | Runs the given parser on a string and returns the resulting JValue or error
runJsonParser :: String -> Parser JValue -> Either ParseError JValue
runJsonParser s p = parse p "" s

-- | Runs jValueParser on the given string and returns the resulting JValue or an error
parseJson :: String -> Either ParseError JValue
parseJson s = runJsonParser s jValueParser
