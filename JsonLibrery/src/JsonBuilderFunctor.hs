module JsonBuilderFunctor (parseToken, JsonToken(..), runJsonParser) where


import Data.Char (isDigit)
import Text.Read (readMaybe)
import GHC.Unicode (isSpace)
import GHC.Unicode (toLower)
import Control.Applicative (Alternative(..), (<|>))

data JsonToken = TokenNumber Double | TokenString String | TokenBool Bool | TokenNull | TokenList [JsonToken] | TokenObject [(String, JsonToken)] deriving (Show, Eq)

newtype JsonParser a = JsonParser {runJsonParser :: String -> Maybe (a, String)}

instance Functor JsonParser where
  fmap :: (a -> b) -> JsonParser a -> JsonParser b
  fmap f (JsonParser p) = JsonParser $ \input -> do
    (a, rest) <- p input
    Just (f a, rest)

instance Applicative JsonParser where
  pure :: a -> JsonParser a
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

parseNumber :: JsonParser JsonToken
parseNumber = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (numStr, rest2) = case rest1 of
                          ('-':rest) -> let (n, r) = span isDigit rest
                                        in ('-':n, r)
                          _ -> span isDigit rest1
  in case readMaybe numStr of
    Just num -> Just (TokenNumber num, rest2)
    _ -> Nothing

parseBool :: JsonParser JsonToken
parseBool = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (boolStr, rest2) = span (not . isSpace) rest1
  in case map toLower boolStr of
    "true" -> Just (TokenBool True, rest2)
    "false" -> Just (TokenBool False, rest2)
    _ -> Nothing


parseNull :: JsonParser JsonToken
parseNull = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (nullStr, rest2) = span (not . isSpace) rest1
  in case nullStr of
    "null" -> Just (TokenNull, rest2)
    _ -> Nothing


parseString :: JsonParser JsonToken
parseString = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (str, rest2) = case rest1 of
                       '\"':rest -> span (/= '\"') rest
                       _         -> span (/= '\"') rest1
  in case rest2 of
       '\"':rest -> Just (TokenString str, rest)
       _         -> Nothing


parseList :: JsonParser JsonToken
parseList = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
      (list, rest2) = case rest1 of
                        '[':rest -> span (/= ']') rest
                        _        -> span (/= ']') rest1
      tokens = sepBy parseToken "," list
  in case rest2 of
       ']':rest -> Just (TokenList tokens, rest)
       _        -> Nothing

sepBy :: JsonParser a -> String -> String -> [a]
sepBy p sep input =
  let (first, rest) = span (/= head sep) input
      remaining = dropWhile (== head sep) rest
  in case runJsonParser p first of
       Nothing -> []
       Just (x, _) ->
         if null remaining
         then [x]
         else x : sepBy p sep remaining

-- > ghci runJsonParser parseList "[1,true,false,null,\"string\",[1,2,3],{age: 20, name: \"John\"}}]" 
parseToken :: JsonParser JsonToken
parseToken = parseString <|> parseNumber <|> parseBool <|> parseNull <|> parseList 
