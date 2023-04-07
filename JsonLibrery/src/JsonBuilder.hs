module JsonBuilder (fromJson) where

import Data.Char (isDigit, isSpace)
import GHC.Unicode (toLower)
import JsonObject (JsonValue (..))

fromJson :: String -> Maybe JsonValue
fromJson s = parseJsonValue (strip s)

parseJsonValue :: String -> Maybe JsonValue
parseJsonValue s
  | null s = Nothing
  | head s == ' ' = parseJsonValue (tail s) -- Ignorar espacios en blanco iniciales
  | last s == ' ' = parseJsonValue (init s) -- Ignorar espacios en blanco finales
  | head s == '{' && last s == '}' = parseJsonObject (init (tail s)) -- Manejar objetos
  | head s == '[' && last s == ']' = parseJsonArray (init (tail s)) -- Manejar listas
  | head s == '\"' && last s == '\"' = parseJsonString (init (tail s)) -- Manejar strings
  | map toLower s == "true" = Just (JBool True)
  | map toLower s == "false" = Just (JBool False)
  | head s == 'n' = parseNull s -- Manejar nulos
  | isDigit (head s) || head s == '-' = parseJsonNumber s -- Manejar números
  | otherwise = Nothing

parseJsonObject :: [Char] -> Maybe JsonValue
parseJsonObject s
  | null s = Just (JObject [])
  | otherwise = Just (JObject (readObjectJson (trim s)))

readObjectJson :: String -> [(String, JsonValue)]
readObjectJson "" = []
readObjectJson s = readObjectJson' s 0 [] []
  where
    readObjectJson' :: [Char] -> Int -> [Char] -> [Char] -> [(String, JsonValue)]
    readObjectJson' "" _ key value = [(key, readValueJson (reverse value))]
    readObjectJson' (c : cs) count key value
      | c == '{' = readObjectJson' cs (count + 1) key (c : value)
      | c == '}' && count == 0 = [(key, readValueJson (reverse value))]
      | c == '}' = readObjectJson' cs (count - 1) key (c : value)
      | c == ',' && count == 0 = (key, readValueJson (reverse value)) : readObjectJson' cs count [] []
      | c == ':' && count == 0 = readObjectJson' cs count (reverse value) []
      | c == '[' = readObjectJson' cs (count + 1) key (c : value)
      | c == ']' = readObjectJson' cs (count - 1) key (c : value)
      | otherwise = readObjectJson' cs count key (c : value)

parseJsonArray :: [Char] -> Maybe JsonValue
parseJsonArray s
  | null s = Just (JList [])
  | otherwise = Just (JList (readArrayJson (trim s)))

readArrayJson :: [Char] -> [JsonValue]
readArrayJson "" = []
readArrayJson s = readArrayJson' s 0 []
  where
    readArrayJson' :: [Char] -> Int -> [Char] -> [JsonValue]
    readArrayJson' "" _ buffer = [readValueJson (reverse buffer)]
    readArrayJson' (c : cs) count buffer
      | c == '[' = readArrayJson' cs (count + 1) (c : buffer)
      | c == ']' && count == 0 = readValueJson (reverse buffer) : readArrayJson' cs count []
      | c == ']' = readArrayJson' cs (count - 1) (c : buffer)
      | c == ',' && count == 0 = readValueJson (reverse buffer) : readArrayJson' cs count []
      | c == '{' = readArrayJson' cs (count + 1) (c : buffer)
      | c == '}' = readArrayJson' cs (count - 1) (c : buffer)
      | otherwise = readArrayJson' cs count (c : buffer)

trim :: [Char] -> [Char]
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

readValueJson :: [Char] -> JsonValue
readValueJson s =
  case parseJsonValue s of
    Just value -> value
    Nothing -> JNull

parseJsonString :: [Char] -> Maybe JsonValue
parseJsonString s = Just (JString s)

parseNull :: [Char] -> Maybe JsonValue
parseNull s
  | s == "null" = Just JNull
  | otherwise = Nothing

parseJsonNumber :: [Char] -> Maybe JsonValue
parseJsonNumber s
  | null wholePart = Nothing -- no se encontró ninguna parte entera
  | null decimalPart = Just (JNumber (read wholePart :: Double)) -- no hay parte decimal
  | length wholePart == 1 && head wholePart == '-' = Nothing -- solo hay un guión en la parte entera
  | head wholePart == '0' && length wholePart > 1 = Nothing -- hay ceros a la izquierda en la parte entera
  | all isDigit decimalPart = Just (JNumber (read (wholePart ++ "." ++ decimalPart) :: Double))
  | otherwise = Nothing
  where
    (wholePart, rest) = span (\c -> isDigit c || c == '-') s
    decimalPart = if rest /= [] && head rest == '.' then takeWhile isDigit (tail rest) else ""

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
