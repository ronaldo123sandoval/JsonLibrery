module TestJsonWhite(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import JsonObject(JsonValue(..))
import JsonWriter(writeJson)


testWriteJsonBoolean :: Assertion
testWriteJsonBoolean = assertEqual "testWriteJson" (writeJson (JObject [("isMarried", JBool True)])) "{isMarried: true}"


testWriteJsonString :: Assertion
testWriteJsonString = assertEqual "testWriteJson" (writeJson (JObject [("name", JString "John")])) "{name: \"John\"}"

testWriteJsonNumber :: Assertion
testWriteJsonNumber = assertEqual "testWriteJson" (writeJson (JObject [("age", JNumber 30)])) "{age: 30.0}"
-- whiteJson JObject [("age", JNumber 30)]

testWriteJsonNull :: Assertion
testWriteJsonNull = assertEqual "testWriteJson" (writeJson (JObject [("isMarried", JNull)])) "{isMarried: null}"

testWriteJsonList :: Assertion
testWriteJsonList = assertEqual "testWriteJson" (writeJson (JObject [("hobbies", JList [JString "reading", JString "playing soccer"])])) "{hobbies: [\"reading\", \"playing soccer\"]}"

testWhiteComplet:: Assertion
testWhiteComplet = assertEqual "testWriteJson" (writeJson (JObject [("name", JString "John"), ("age", JNumber 30), ("isMarried", JBool True), ("hobbies", JList [JString "reading", JString "playing soccer"])])) "{name: \"John\", age: 30.0, isMarried: true, hobbies: [\"reading\", \"playing soccer\"]}"

tests :: TestTree
tests = testGroup "Builder Tests"
    [ testCase "testWriteJsonBoolean" testWriteJsonBoolean
    , testCase "testWriteJsonString" testWriteJsonString
    , testCase "testWriteJsonNumber" testWriteJsonNumber
    , testCase "testWriteJsonNull" testWriteJsonNull
    , testCase "testWriteJsonList" testWriteJsonList
    , testCase "testWhiteComplet" testWhiteComplet
    ]

