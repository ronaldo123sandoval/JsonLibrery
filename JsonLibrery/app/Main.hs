module Main (main) where
import JsonBuilder (fromJson)
import Data.Maybe (fromMaybe)
import JsonObject(JsonValue(..))
import JsonWriter(writeJson)
import JsonBuilderFunctor (parseToken,runJsonParser)
main :: IO ()
main = do
  putStrLn "***************************************"
  putStrLn "Prueba 1: String a JsonValue sin Funtor"
  let test1 = " {test1:\"test\",object1:{test2:[\"abc\",[1,2,3],\"abc[d]\"],object2:{name:\"value\",test3:\"value3\"}}}"
  let test1Json = fromMaybe (error "Error") (fromJson test1)
  putStrLn "***************************************"
  putStrLn $ "Sin functor: " ++ show test1Json
  putStrLn "***************************************"
  putStrLn "Prueba 2: String a JsonValue con Funtor"
  let test2 = "{\"test1\":\"test\",\"object1\":{\"test2\":[\"abc\",[1,2,3],\"abc[d]\"],\"object2\":{\"name\":\"value\",\"test3\":\"value3\"}}}"
  let test2Json = runJsonParser parseToken test2
  putStrLn "***************************************"
  putStrLn $ "Con functor: " ++ show test2Json
  putStrLn "***************************************"
  putStrLn "Prueba 3: JsonValue a String"
  let test3 = writeJson (JObject [("test1", JString "test"), ("object1", JObject [("test2", JList [JString "abc", JList [JNumber 1.0, JNumber 2.0, JNumber 3.0], JString "abc[d]"]), ("object2", JObject [("name", JString "value"), ("test3", JString "value3")])])])
  putStrLn "***************************************"
  putStrLn $ "WhiteJson: " ++ test3

