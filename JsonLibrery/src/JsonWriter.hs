module JsonWriter (writeJson) where

import Data.List (intercalate)
import JsonObject (JsonValue (..))

writeJson :: JsonValue -> String
writeJson (JString s) = "\"" ++ s ++ "\""
writeJson (JBool b) = if b then "true" else "false"
writeJson (JNumber n) = show n
writeJson (JList l) = "[" ++ intercalate ", " (map writeJson l) ++ "]"
writeJson (JObject o) = "{" ++ intercalate ", " (map showNode o) ++ "}"
writeJson JNull = "null"

showNode :: (String, JsonValue) -> String
showNode (name, value) = name ++ ": " ++ writeJson value
