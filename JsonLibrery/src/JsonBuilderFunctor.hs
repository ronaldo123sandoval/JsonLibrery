module JsonBuilderFunctor (parseToken, JsonToken (..), runJsonParser) where

import Control.Applicative
import Data.Char (isDigit)
import GHC.Unicode (isSpace)
import Text.Read (readMaybe)

data JsonToken
  = TokenNumber Double
  | TokenString String
  | TokenBool Bool
  | TokenNull
  | TokenList [JsonToken]
  | TokenObject [(String, JsonToken)]
  deriving (Show, Eq)

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
  let (numStr, rest) = span (\c -> isDigit c || c == '.' || c == '-') input
   in case readMaybe numStr of
        Just num -> Just (TokenNumber num, rest)
        Nothing -> Nothing

parseBool :: JsonParser JsonToken
parseBool = JsonParser $ \input ->
  case input of
    't' : 'r' : 'u' : 'e' : rest -> Just (TokenBool True, rest)
    'f' : 'a' : 'l' : 's' : 'e' : rest -> Just (TokenBool False, rest)
    _ -> Nothing

parseNull :: JsonParser JsonToken
parseNull = JsonParser $ \input ->
  case input of
    'n' : 'u' : 'l' : 'l' : rest -> Just (TokenNull, rest)
    _ -> Nothing

parseString :: JsonParser JsonToken
parseString = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
   in case rest1 of
        '"' : rest ->
          let (str, rest2) = span (/= '"') rest
           in Just (TokenString str, tail rest2)
        _ -> Nothing

parseList :: JsonParser JsonToken
parseList = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
   in case rest1 of
        '[' : rest2 ->
          case runJsonParser parseListAux rest2 of
            Just (list, rest3) -> Just (TokenList list, rest3)
            Nothing -> Nothing
        _ -> Nothing

parseListAux :: JsonParser [JsonToken]
parseListAux = JsonParser $ \input ->
  let (_, rest1) = span isSpace input
   in case rest1 of
        ']' : rest -> Just ([], rest)
        _ ->
          case runJsonParser parseToken rest1 of
            Just (token, ',' : rest2) ->
              case runJsonParser parseListAux rest2 of
                Just (list, rest3) -> Just (token : list, rest3)
                Nothing -> Nothing
            Just (token, rest2) ->
              case rest2 of
                ',' : rest3 -> Just ([token], rest3)
                ']' : rest3 -> Just ([token], rest3)
                _ -> Nothing
            Nothing -> Nothing

-- > ghci runJsonParser parseList "[1,true,false,null,\"string\",[1,2,3],{age: 20, name: \"John\"}}]"
parseToken :: JsonParser JsonToken
parseToken = parseString <|> parseNumber <|> parseBool <|> parseNull <|> parseList 
