module Main (main) where
import JsonBuilder (fromJson)
import JsonWriter (writeJson)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  putStrLn "Ingrese un número:"
  putStrLn "1: Función que convierte un string en un JsonValue"
  putStrLn "2: Función que convierte un JsonValue en un string"
  putStrLn "3: Salir"
  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "Ingrese un string:"
      string <- getLine
      let json = fromMaybe (error "Error al parsear el JSON") (fromJson string)
      putStrLn "JSON parseado:"
      print json
      main
    "2" -> do
      putStrLn "Ingrese un string:"
      string <- getLine
      let json = fromMaybe (error "Error al parsear el JSON") (fromJson string)
      putStrLn "JSON en formato de string:"
      putStrLn (writeJson json)
      main
    "3" -> do
      putStrLn "Adiós"
    _ -> do
      putStrLn "Opción inválida"
      main
-- Nota: Por alguna razon el main añade \ al string y el formato sale un poco raro,Si lo corre por el ghci no aparecen los \. 
-- (Razon.....Pues no lo se (╥﹏╥) , lo arreglaria pero si no descanso tendre pesadillas con los strings (っ˘̩╭╮˘̩)っ ) sorry 。゜゜(´Ｏ`) ゜゜。
-- PD: Hay varios test con varios casos de prueba.Este main lo arreglare cuando actualice el proyecto a la version con los functors ಠ_ಠ
-- fromJson "{test1:\"test\",object1:{test2:[\"abc\",[1,2,3],\"abc[d]\"],object2:{name:\"value\",test3:\"value3\"}}}"
-- Resultado en el GHCI: Just (JObject [("test1",JString "test"),("object1",JObject [("test2",JList [JString "abc",JList [JNumber 1.0,JNumber 2.0,JNumber 3.0],JString "abc[d]"]),("object2",JObject [("name",JString "value"),("test3",JString "value3")])])])
