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
  (<*>) :: JsonParser (a -> b) -> JsonParser a -> JsonParser b
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

-- instance Alternative JsonParser where
--   empty = JsonParser (\x -> Nothing)
--   p1 <|> p2 = JsonParser (\)
parseNumber :: JsonParser JsonToken
parseNumber = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (numStr, rest2) = case rest1 of
                          ('-':rest) -> let (n, r) = span isDigit rest
                                        in ('-':n, r)
                          _ -> span isDigit rest1
  in case readMaybe numStr of
    Nothing -> Nothing
    Just num -> Just (TokenNumber num, rest2)
-- correguir el parse number:
-- runJsonParser parseNumber "123true"  este caso no es valido y tiene que salir Nothing
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

parseList :: JsonParser JsonToken
parseList = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (_, rest2) = span (/= ']') (tail rest1)
  in if head rest2 == ']'
       then Just (TokenList [], tail rest2)
       else Nothing

parseToken :: JsonParser JsonToken
parseToken = parseNumber <|> parseBool <|> parseNull <|> parseString <|> parseList
