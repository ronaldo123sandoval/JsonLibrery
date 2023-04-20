module JsonBuilderFunctor (parseToken, JsonToken(..), runJsonParser) where


import Data.Char (isDigit)
import Text.Read (readMaybe)
import GHC.Unicode (isSpace)
import GHC.Unicode (toLower)
import Control.Applicative (Alternative(..), (<|>))

data JsonToken = TokenNumber Double | TokenString String | TokenBool Bool | TokenNull | TokenList [JsonToken] | TokenObject [(String, JsonToken)] deriving (Show, Eq)

newtype JsonParser a = JsonParser {runJsonParser :: String -> Maybe (a, String)}

instance Functor JsonParser where
  fmap f p = JsonParser $ \input -> do
    (a, rest) <- runJsonParser p input
    Just (f a, rest)

instance Applicative JsonParser where
  pure a = JsonParser $ \input -> Just (a, input)
  (JsonParser f) <*> (JsonParser a) = JsonParser $ \input -> do
    (func, rest1) <- f input
    (val, rest2) <- a rest1
    Just (func val, rest2)

instance Alternative JsonParser where
  empty = JsonParser $ \_ -> Nothing
  p1 <|> p2 = JsonParser $ \input ->
    case runJsonParser p1 input of
      Nothing -> runJsonParser p2 input
      Just (a, rest) -> Just (a, rest)

parseNumber :: JsonParser JsonToken
parseNumber = JsonParser $ \input ->
  let (numberStr, rest) = span isDigit input
  in case readMaybe numberStr of
    Just number -> Just (TokenNumber number, rest)
    Nothing -> Nothing

parseBool :: JsonParser JsonToken
parseBool = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (boolStr, rest2) = span (not . isSpace) rest1
  in if map toLower boolStr == "true"
      then Just (TokenBool True, rest2)
      else if map toLower boolStr == "false"
        then Just (TokenBool False, rest2)
        else Nothing

parseNull :: JsonParser JsonToken
parseNull = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (nullStr, rest2) = span (not . isSpace) rest1
  in if nullStr == "null"
      then Just (TokenNull, rest2)
      else Nothing

parseString :: JsonParser JsonToken
parseString = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (str, rest2) = if head rest1 == '\"'
                       then span (/= '\"') (tail rest1)
                       else span (/= '\"') rest1
  in if head rest2 == '\"'
       then Just (TokenString str, tail rest2)
       else Nothing

parseToken :: JsonParser JsonToken
parseToken = parseNumber <|> parseBool <|> parseNull <|> parseString

-- ghci runJsonParser parseToken "[1, 2, 3]"