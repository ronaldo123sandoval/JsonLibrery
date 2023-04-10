module JsonFunctors(JsonValueFunctor(..)) where
import Data.Char (isDigit, isSpace)
import GHC.Unicode (toLower)
-- Definición del tipo de datos JsonValueFunctor
data JsonValueFunctor a = JStringFunctor a
                        | JNumberFunctor a
                        | JBoolFunctor a
                        | JNullFunctor a
                        | JObjectFunctor [(String, a)]
                        | JListFunctor  [a]
                        deriving (Show, Eq)

instance Functor JsonValueFunctor where
    fmap f (JStringFunctor a) = JStringFunctor (f a)
    fmap f (JNumberFunctor a) = JNumberFunctor (f a)
    fmap f (JBoolFunctor a) = JBoolFunctor (f a)
    fmap f (JNullFunctor a) = JNullFunctor (f a)
    fmap f (JObjectFunctor a) = JObjectFunctor (map (\(x,y) -> (x, f y)) a)
    fmap f (JListFunctor a) = JListFunctor (map f a)

fromJson :: String -> Maybe (JsonValueFunctor String)
fromJson s = parseJsonValue (strip s)

parseJsonValue :: String -> Maybe (JsonValueFunctor String)
parseJsonValue s
  | null s = Nothing
  | head s == ' ' = parseJsonValue (tail s) -- Ignorar espacios en blanco iniciales
  | last s == ' ' = parseJsonValue (init s) -- Ignorar espacios en blanco finales
  -- | head s == '{' && last s == '}' = parseJsonObject (init (tail s)) -- Manejar objetos
--  | head s == '[' && last s == ']' = parseJsonArray (init (tail s)) -- Manejar listas
  | head s == '\"' && last s == '\"' = parseJsonString (init (tail s)) -- Manejar strings
  | map toLower s == "true" = Just (JBoolFunctor "true")
  | map toLower s == "false" = Just (JBoolFunctor "false")
  | head s == 'n' = parseNull s -- Manejar nulos
  | isDigit (head s) || head s == '-' = parseJsonNumber s -- Manejar números
  | otherwise = Nothing

parseJsonString :: String -> Maybe (JsonValueFunctor String)
parseJsonString s = Just (JStringFunctor s)

parseJsonNumber :: String -> Maybe (JsonValueFunctor String)
parseJsonNumber s
  | all isDigit s = Just (JNumberFunctor s)
  | otherwise = Nothing
  
parseNull :: String -> Maybe (JsonValueFunctor String)
parseNull s
  | map toLower s == "null" = Just (JNullFunctor "null")
  | otherwise = Nothing



strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
