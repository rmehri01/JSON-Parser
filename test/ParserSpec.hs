import           Test.Tasty
import           Test.Tasty.SmallCheck         as SC
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.HUnit

import           Parser
import           Data.Char                      ( toLower )
import           System.Environment

main :: IO ()
main = do
    -- setEnv "TASTY_QUICKCHECK_VERBOSE" "true"
    -- setEnv "TASTY_QUICKCHECK_TESTS" "10000"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup
    "(checked by SmallCheck)"
    [ SC.testProperty "lowercase boolean is parsed correctly" $ \bool ->
          runJsonParser (map toLower $ show bool) jBoolParser
              == Right (JBool bool)
    ]

qcProps = testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "any given double can be parsed correctly" $ \double ->
        runJsonParser (show double) jNumberParser == Right (JNumber double)
    , QC.testProperty "any integer can be parsed and is converted to a double"
        $ \int -> runJsonParser (show (int :: Integer)) jNumberParser
              == Right (JNumber (fromIntegral int))
    ]

unitTests = testGroup
    "Unit tests"
    [ testCase "lowercase null parsed correctly"
      $   runJsonParser "null" jNullParser
      @?= Right JNull
    ]
