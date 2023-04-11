module JsonFunctors (fromJsonFunctor) where

import JsonObject (JsonValue (..))
import JsonBuilder (fromJson)

newtype JsonFunctor a = JsonFunctor { runJsonFunctor :: JsonValue }

instance Functor JsonFunctor where
  fmap _ (JsonFunctor (JString s)) = JsonFunctor $ JString s
  fmap _ (JsonFunctor (JNumber n)) = JsonFunctor $ JNumber n
  fmap _ (JsonFunctor (JBool b)) = JsonFunctor $ JBool b
  fmap _ (JsonFunctor JNull) = JsonFunctor JNull
  fmap f (JsonFunctor (JObject o)) = JsonFunctor $ JObject [(k, runJsonFunctor $ fmap f $ JsonFunctor v) | (k, v) <- o]
  fmap f (JsonFunctor (JList l)) = JsonFunctor $ JList $ map (runJsonFunctor . fmap f . JsonFunctor) l

fromJsonFunctor :: String -> Maybe (JsonFunctor a)
fromJsonFunctor s = fmap JsonFunctor $ fromJson s

-- Definición de instancia Show para JsonFunctor
instance Show (JsonFunctor a) where
  show (JsonFunctor jv) = show jv
