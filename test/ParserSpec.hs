import           Test.Tasty
import           Test.Tasty.SmallCheck         as SC
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.HUnit

import           Parser
import           Data.Char                      ( toLower )
import           Data.Either                    ( isLeft )
import           Data.List                      ( isInfixOf )
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
    [ QC.testProperty "any input that is not 'null' is not parsed" $ \string ->
        string /= "null" QC.==> isLeft (runJsonParser string jNullParser)
    , QC.testProperty "any input that is not 'true' or 'false' is not parsed"
        $ \string -> (string /= "true" && string /= "false")
              QC.==> isLeft (runJsonParser string jBoolParser)
    , QC.testProperty "any given double can be parsed correctly" $ \double ->
        runJsonParser (show double) jNumberParser == Right (JNumber double)
    , QC.testProperty "any integer can be parsed and is converted to a double"
        $ \int -> runJsonParser (show (int :: Integer)) jNumberParser
              == Right (JNumber (fromIntegral int))
    , QC.testProperty
            "any non-quoted string between can be parsed when put between quotes"
        $ \string ->
              not ("\"" `isInfixOf` string)
                  QC.==> (  runJsonParser ("\"" ++ string ++ "\"") jStringParser
                         == Right (JString string)
                         )
    ]

unitTests = testGroup
    "Unit tests"
    [ testCase "lowercase null parses correctly"
    $   runJsonParser "null" jNullParser
    @?= Right JNull
    , testCase "array with any amount of whitespace parses correctly"
    $   runJsonParser "[ 1  ,   true ,  null  ,\"hello\"]" jArrayParser
    @?= Right (JArray [JNumber 1.0, JBool True, JNull, JString "hello"])
    , testCase "array with missing bracket should fail"
    $   isLeft (runJsonParser "1, true, null, \"hello\"]" jArrayParser)
    @?= True
    , testCase "array with missing comma should fail"
    $   isLeft (runJsonParser "[1, true, null \"hello\"]" jArrayParser)
    @?= True
    , testCase "object with any amount of whitespace parses correctly"
    $   runJsonParser "{  \"first\"   : 24  ,\"second\" :   null}" jObjectParser
    @?= Right (JObject [("first", JNumber 24.0), ("second", JNull)])
    , testCase "object with missing bracket should fail"
    $   isLeft
            (runJsonParser "  \"first\"   : 24  ,\"second\" :   null}"
                           jObjectParser
            )
    @?= True
    , testCase "object with missing comma should fail"
    $   isLeft
            (runJsonParser "{  \"first\"   : 24  \"second\" :   null}"
                           jObjectParser
            )
    @?= True
    , testCase "object with missing colon should fail"
    $   isLeft
            (runJsonParser "{  \"first\"   24 ,  \"second\" :   null}"
                           jObjectParser
            )
    @?= True
    , testCase "any jValue that is arbitrarily nested can be parsed"
    $   runJsonParser
            "[\"first\" ,-24.6 , { \"second\" : [ true, false, \"hello\" ] } , [null, 23]]"
            jValueParser
    @?= Right
            (JArray
                [ JString "first"
                , JNumber (-24.6)
                , JObject
                    [ ( "second"
                      , JArray [JBool True, JBool False, JString "hello"]
                      )
                    ]
                , JArray [JNull, JNumber 23.0]
                ]
            )
    ]
