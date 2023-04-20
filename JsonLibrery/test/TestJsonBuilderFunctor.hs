module TestJsonBuilderFunctor (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import JsonBuilderFunctor (parseToken, JsonToken(..), runJsonParser)

testParseTokenString :: Assertion
testParseTokenString = assertEqual "testParseTokenString" (runJsonParser parseToken "\"hello\"") (Just (TokenString "hello", ""))

testParseTokenStringWithSpace :: Assertion
testParseTokenStringWithSpace = assertEqual "testParseTokenStringWithSpace" (runJsonParser parseToken "\"hello world\"") (Just (TokenString "hello world", ""))

testParseTokenStringWithNumber :: Assertion
testParseTokenStringWithNumber = assertEqual "testParseTokenStringWithNumber" (runJsonParser parseToken "\"hello, 123\"")  (Just (TokenString "hello, 123", ""))

testParseTokenNumber :: Assertion
testParseTokenNumber = assertEqual "testParseTokenNumber" (runJsonParser parseToken "123") (Just (TokenNumber 123, ""))

testParseTokenNumberWithSpace :: Assertion
testParseTokenNumberWithSpace =  assertEqual "testParseTokenNumber" (runJsonParser parseToken "1  2 3") (Just (TokenNumber 1.0, "  2 3"))

testParseTokenNumberWithNull :: Assertion
testParseTokenNumberWithNull =  assertEqual "testParseTokenNumberWithNull" (runJsonParser parseToken "1 null") (Just (TokenNumber 1.0, " null"))

testParseTokenNumbeNegative :: Assertion
testParseTokenNumbeNegative =  assertEqual "testParseTokenNumbeNegative" (runJsonParser parseToken "-1") (Just (TokenNumber (-1.0), ""))

testParseTokenBool :: Assertion
testParseTokenBool =  assertEqual "testParseTokenBool" (runJsonParser parseToken "true") (Just (TokenBool True, ""))

testParseTokenBoollWithSpace :: Assertion
testParseTokenBoollWithSpace =  assertEqual "testParseTokenBollWithSpace" (runJsonParser parseToken "true  false") (Just (TokenBool True, "  false"))

testParseTokenBoolWithCapitalLetter :: Assertion
testParseTokenBoolWithCapitalLetter =  assertEqual "testParseTokenBoolWithCapitalLetter" (runJsonParser parseToken "FALSE") (Just (TokenBool False, ""))

testParseNUll :: Assertion
testParseNUll =  assertEqual "testParseNUll" (runJsonParser parseToken "null") (Just (TokenNull, ""))

testParseList :: Assertion
testParseList =  assertEqual "testParseList" (runJsonParser parseToken "[1, 2, 3]") (Just (TokenList [TokenNumber 1.0, TokenNumber 2.0, TokenNumber 3.0], ""))

testParseListAllType :: Assertion
testParseListAllType =  assertEqual "testParseListAllType" (runJsonParser parseToken "[1, \"hello\", true, null]") (Just (TokenList [TokenNumber 1.0, TokenString "hello", TokenBool True, TokenNull], ""))

testParseListWithObject :: Assertion
testParseListWithObject = assertEqual "testParseListWithObject" (runJsonParser parseToken "[1, \"hello\", true, null, {\"name\": \"John\"}]") (Just (TokenList [TokenNumber 1.0, TokenString "hello", TokenBool True, TokenNull, TokenObject [("name", TokenString "John")]], ""))

testParseListIntoList :: Assertion
testParseListIntoList = assertEqual "testParseListIntoList" (runJsonParser parseToken "[1, \"hello\", true, null, [1, 2, 3]]") (Just (TokenList [TokenNumber 1.0, TokenString "hello", TokenBool True, TokenNull, TokenList [TokenNumber 1.0, TokenNumber 2.0, TokenNumber 3.0]], ""))

testParseObject :: Assertion
testParseObject = assertEqual "testParseObject" (runJsonParser parseToken "{\"name\": \"John\", \"age\": 30}") (Just (TokenObject [("name", TokenString "John"), ("age", TokenNumber 30.0)], ""))

testParseObjectWithList :: Assertion
testParseObjectWithList = assertEqual "testParseObjectWithList" (runJsonParser parseToken "{\"name\": \"John\", \"age\": 30, \"list\": [1, 2, 3]}") (Just (TokenObject [("name", TokenString "John"), ("age", TokenNumber 30.0), ("list", TokenList [TokenNumber 1.0, TokenNumber 2.0, TokenNumber 3.0])], ""))

testParseObjectWithObject :: Assertion
testParseObjectWithObject = assertEqual "testParseObjectWithObject" (runJsonParser parseToken "{\"name\": \"John\", \"age\": 30, \"obj\": {\"name\": \"John\"}}") (Just (TokenObject [("name", TokenString "John"), ("age", TokenNumber 30.0), ("obj", TokenObject [("name", TokenString "John")])], ""))

tests :: TestTree
tests = testGroup "Builder Functor Tests"
    [ testCase "testParseTokenString" testParseTokenString
    , testCase "testParseTokenStringWithSpace" testParseTokenStringWithSpace
    , testCase "testParseTokenStringWithNumber" testParseTokenStringWithNumber
    , testCase "testParseTokenNumber" testParseTokenNumber
    , testCase "testParseTokenNumberWithSpace" testParseTokenNumberWithSpace
    , testCase "testParseTokenNumberWithNull" testParseTokenNumberWithNull
    , testCase "testParseTokenNumbeNegative" testParseTokenNumbeNegative
    , testCase "testParseTokenBool" testParseTokenBool
    , testCase "testParseTokenBoollWithSpace" testParseTokenBoollWithSpace
    , testCase "testParseTokenBoolWithCapitalLetter" testParseTokenBoolWithCapitalLetter
    , testCase "testParseNUll" testParseNUll
    , testCase "testParseList" testParseList
    , testCase "testParseListAllType" testParseListAllType
    , testCase "testParseListWithObject" testParseListWithObject
    , testCase "testParseListIntoList" testParseListIntoList
    , testCase "testParseObject" testParseObject
    , testCase "testParseObjectWithList" testParseObjectWithList
    , testCase "testParseObjectWithObject" testParseObjectWithObject
    ]
