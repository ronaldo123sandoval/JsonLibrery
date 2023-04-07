import Test.Tasty

import TestJsonWhite
import TestJsonBuilder

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" 
  [ 
    TestJsonWhite.tests 
  , TestJsonBuilder.tests
  ]
