module TestJsonBuilder(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import JsonObject(JsonValue(..))
import JsonBuilder(fromJson)

testFromJsonString :: Assertion
testFromJsonString = assertEqual "testFromJsonWithString" (fromJson "\"Hello, World!\"" ) (Just (JString "Hello, World!"))

jsonStringTestWithSpaces:: Assertion
jsonStringTestWithSpaces = assertEqual "testFromJsonWithStringAndSpaces" (fromJson "  \"Hello, World!\"  " ) (Just (JString "Hello, World!"))

testFromJsonBooleanTrue :: Assertion
testFromJsonBooleanTrue = assertEqual "testFromJsonWithBoolean" (fromJson "true" ) (Just (JBool True))

jsonBooleanTrueTestWithSpaces :: Assertion
jsonBooleanTrueTestWithSpaces = assertEqual "testFromJsonWithBooleanAndSpaces" (fromJson "  true  " ) (Just (JBool True))

testBooleanTrueJsonWithUppercase :: Assertion
testBooleanTrueJsonWithUppercase = assertEqual "testFromJsonWithUppercaseBoolean" (fromJson "TRUE" ) (Just (JBool True))

testFromJsonBooleanFalse :: Assertion
testFromJsonBooleanFalse = assertEqual "testFromJsonWithBoolean" (fromJson "false" ) (Just (JBool False))

jsonBooleanFalseTestWithSpaces :: Assertion
jsonBooleanFalseTestWithSpaces = assertEqual "testFromJsonWithBooleanAndSpaces" (fromJson "  false  " ) (Just (JBool False))

testBooleanFalseJsonWithUppercase :: Assertion
testBooleanFalseJsonWithUppercase = assertEqual "testFromJsonWithUppercaseBoolean" (fromJson "FALSE" ) (Just (JBool False))

testFromJsonPositiveNumber :: Assertion
testFromJsonPositiveNumber = assertEqual "testFromJsonWithNumber" (fromJson "12" ) (Just (JNumber 12))

testFromJsonNegativeNumber :: Assertion
testFromJsonNegativeNumber = assertEqual "testFromJsonWithNegativeNumber" (fromJson "-12" ) (Just (JNumber (-12)))

testFromJsonDoubleNumber :: Assertion
testFromJsonDoubleNumber = assertEqual "testFromJsonWithDoubleNumber" (fromJson "12.34" ) (Just (JNumber 12.34))

jsonNumberTestWithSpaces :: Assertion
jsonNumberTestWithSpaces = assertEqual "testFromJsonWithNumberAndSpaces" (fromJson "  12  " ) (Just (JNumber 12))

testFromJsonNull :: Assertion
testFromJsonNull = assertEqual "testFromJsonWithNull" (fromJson "null" ) (Just JNull) 

jsonNullTestWithSpaces :: Assertion
jsonNullTestWithSpaces = assertEqual "testFromJsonWithNullAndSpaces" (fromJson "  null  " ) (Just JNull)

testFromJsonList :: Assertion
testFromJsonList = assertEqual "testFromJsonWithList" (fromJson "[1,2,3]" ) (Just (JList [JNumber 1, JNumber 2, JNumber 3]))

testFromJsonListAlltypes :: Assertion
testFromJsonListAlltypes = assertEqual "testFromJsonWithListAlltypes" (fromJson "[1, \"Hello\", true, false, null]" ) (Just (JList [JNumber 1, JString "Hello", JBool True, JBool False, JNull]))

testFromJsonListWithSpaces :: Assertion
testFromJsonListWithSpaces = assertEqual "testFromJsonWithListAndSpaces" (fromJson "  [1, 2, 3]  " ) (Just (JList [JNumber 1, JNumber 2, JNumber 3]))

testFromJsonListInList :: Assertion
testFromJsonListInList = assertEqual "testFromJsonWithListInList" (fromJson "[[1,2,3], [4,5,6]]" ) (Just (JList [JList [JNumber 1, JNumber 2, JNumber 3], JList [JNumber 4, JNumber 5, JNumber 6]]))

testFromJsonObjectString :: Assertion
testFromJsonObjectString = assertEqual "testFromJsonObjectString" (fromJson "{name: \"John\"}" ) (Just (JObject [("name", JString "John")]))

testFromJsonObjectNumber :: Assertion
testFromJsonObjectNumber = assertEqual "testFromJsonObjectNumber" (fromJson "{age: 12}" ) (Just (JObject [("age", JNumber 12)]))

testFromJsonObjectBoolean :: Assertion
testFromJsonObjectBoolean = assertEqual "testFromJsonObjectBoolean" (fromJson "{isAlive: true}" ) (Just (JObject [("isAlive", JBool True)]))

testFromJsonObjectNull :: Assertion
testFromJsonObjectNull = assertEqual "testFromJsonObjectNull" (fromJson "{isAlive: null}" ) (Just (JObject [("isAlive", JNull)]))

testFromJsonObjectList :: Assertion
testFromJsonObjectList = assertEqual "testFromJsonObjectList" (fromJson "{numbers: [1,2,3]}" ) (Just (JObject [("numbers", JList [JNumber 1, JNumber 2, JNumber 3])]))

testFromJsonObjectObject :: Assertion
testFromJsonObjectObject = assertEqual "testFromJsonObjectObject" (fromJson "{person: {name: \"John\",age: 12}}" ) (Just (JObject [("person", JObject [("name", JString "John"), ("age", JNumber 12)])]))

testFromJsonObjectWithSpaces :: Assertion
testFromJsonObjectWithSpaces = assertEqual "testFromJsonObjectWithSpaces" (fromJson "  {name: \"John\"}  " ) (Just (JObject [("name", JString "John")]))

testFromJsonObjectAllTypesSimple :: Assertion
testFromJsonObjectAllTypesSimple = assertEqual "testFromJsonObjectAllTypesSimple" (fromJson "{name: \"John\", age: 12, isAlive: true, isDead: false, isNull: null, list:[1,2,3], obj:{name: \"sara\"}}" )  (Just (JObject [("name",JString "John"),(" age",JNumber 12.0),(" isAlive",JBool True),(" isDead",JBool False),(" isNull",JNull),(" list",JList [JNumber 1.0,JNumber 2.0,JNumber 3.0]),(" obj",JObject [("name",JString "sara")])]))

testFromJsonObjectAllTypesComplex :: Assertion
testFromJsonObjectAllTypesComplex = assertEqual "testFromJsonObjectAllTypesComplex" (fromJson "{name: \"John\", age: 12, hobbies:[\"music\",\"videGame\",[TRUE,FALSE],{music:\"salsa\"}],abj:{name: \"sara\",age: 12}}" ) (Just (JObject [("name",JString "John"),(" age",JNumber 12.0),(" hobbies",JList [JString "music",JString "videGame",JList [JBool True,JBool False],JObject [("music",JString "salsa")]]),("abj",JObject [("name",JString "sara"),("age",JNumber 12.0)])]))

tests :: TestTree
tests = testGroup "Builder Tests"
    [ testCase "testFromJsonString" testFromJsonString
    , testCase "jsonStringTestWithSpaces" jsonStringTestWithSpaces
    , testCase "testFromJsonBooleanTrue" testFromJsonBooleanTrue
    , testCase "jsonBooleanTrueTestWithSpaces" jsonBooleanTrueTestWithSpaces
    , testCase "testBooleanTrueJsonWithUppercase" testBooleanTrueJsonWithUppercase
    , testCase "testFromJsonBooleanFalse" testFromJsonBooleanFalse
    , testCase "jsonBooleanFalseTestWithSpaces" jsonBooleanFalseTestWithSpaces
    , testCase "testBooleanFalseJsonWithUppercase" testBooleanFalseJsonWithUppercase
    , testCase "testFromJsonPositiveNumber" testFromJsonPositiveNumber
    , testCase "testFromJsonNegativeNumber" testFromJsonNegativeNumber
    , testCase "testFromJsonDoubleNumber" testFromJsonDoubleNumber
    , testCase "jsonNumberTestWithSpaces" jsonNumberTestWithSpaces
    , testCase "testFromJsonNull" testFromJsonNull
    , testCase "jsonNullTestWithSpaces" jsonNullTestWithSpaces
    , testCase "testFromJsonList" testFromJsonList
    , testCase "testFromJsonListAlltypes" testFromJsonListAlltypes
    , testCase "testFromJsonListWithSpaces" testFromJsonListWithSpaces
    , testCase "testFromJsonListInList" testFromJsonListInList
    , testCase "testFromJsonObjectString" testFromJsonObjectString
    , testCase "testFromJsonObjectNumber" testFromJsonObjectNumber
    , testCase "testFromJsonObjectBoolean" testFromJsonObjectBoolean
    , testCase "testFromJsonObjectNull" testFromJsonObjectNull
    , testCase "testFromJsonObjectList" testFromJsonObjectList
    , testCase "testFromJsonObjectObject" testFromJsonObjectObject
    , testCase "testFromJsonObjectWithSpaces" testFromJsonObjectWithSpaces
    , testCase "testFromJsonObjectAllTypesSimple" testFromJsonObjectAllTypesSimple
    , testCase "testFromJsonObjectAllTypesComplex" testFromJsonObjectAllTypesComplex
    ]