import Test.HUnit
import Data.Hjq (parseJqFilter)
import Data.Hjq.Parser (JqFilter(JqField, JqIndex, JqNil))

main :: IO ()
main = do
    runTestTT $ TestList
        [
            jqFilterParseTest
        ]
    return()

jqFilterParseTest :: Test
jqFilterParseTest = TestList
    [
        "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil
    ]