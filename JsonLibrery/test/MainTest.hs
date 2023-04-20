import Test.Tasty

import TestJsonWhite
import TestJsonBuilder
import TestJsonBuilderFunctor

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" 
  [ 
    TestJsonWhite.tests 
  , TestJsonBuilder.tests
  , TestJsonBuilderFunctor.tests
  ]
