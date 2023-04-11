module JsonBuilderFunctor (fromJsonBuilderFunctor) where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import GHC.Unicode (toLower)
import JsonObject (JsonValue (..))

newtype JsonParser a = JsonParser { runParser :: String -> Maybe (a, String) }

instance Functor JsonParser where
  fmap f (JsonParser p) = JsonParser (\s -> fmap (\(a, s') -> (f a, s')) (p s))

instance Applicative JsonParser where
  pure a = JsonParser (\s -> Just (a, s))
  (JsonParser p1) <*> (JsonParser p2) = JsonParser (\s -> case p1 s of
    Nothing -> Nothing
    Just (f, s') -> fmap (\(a, s'') -> (f a, s'')) (p2 s'))

instance Monad JsonParser where
  return = pure
  (JsonParser p1) >>= f = JsonParser (\s -> case p1 s of
    Nothing -> Nothing
    Just (a, s') -> runParser (f a) s')

instance Alternative JsonParser where
  empty = JsonParser (\_  -> Nothing)
  (JsonParser p1) <|> (JsonParser p2) = JsonParser (\s -> p1 s <|> p2 s)

instance MonadPlus JsonParser where
  mzero = empty
  mplus = (<|>)

instance MonadFail JsonParser where
  fail _ = empty

fromJsonBuilderFunctor :: String -> Maybe JsonValue
fromJsonBuilderFunctor s = case runParser parseJsonValue (strip s) of
  Just (result, _) -> Just result
  Nothing -> Nothing

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseJsonValue :: JsonParser JsonValue
parseJsonValue = do
  s <- JsonParser (\s -> Just (strip s, s))
  case s of
    "" -> empty
    _ -> case head s of
      ' ' -> parseJsonValue
      '}' -> empty
      '\"' -> parseJsonString (init (tail s))
      't' -> if map toLower s == "true" then return (JBool True) else empty
      'f' -> if map toLower s == "false" then return (JBool False) else empty
      'n' -> parseNull s
      _ -> if isDigit (head s) || head s == '-' then parseJsonNumber s else empty

parseNull :: String -> JsonParser JsonValue
parseNull s = if map toLower s == "null" then return JNull else empty

parseJsonNumber :: String -> JsonParser JsonValue
parseJsonNumber s = do
  let (number, _) = span (\c -> isDigit c || c == '.') s
  if number == "" then empty else return (JNumber (read number))

parseJsonString :: String -> JsonParser JsonValue
parseJsonString s = do
  let (string, rest) = span (/= '\"') s
  if rest == "" then empty else return (JString string)
