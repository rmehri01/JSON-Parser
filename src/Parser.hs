module Parser where

import           Text.Parsec

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


