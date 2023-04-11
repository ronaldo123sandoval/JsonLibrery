module JsonObject(JsonValue(..)) where 
data JsonValue = JString String
          | JNumber Double
          | JBool Bool
          | JNull
          | JObject [(String, JsonValue)]
          | JList [JsonValue]
          deriving (Show, Eq)