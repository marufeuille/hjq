{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Data.Hjq.Parser 
main :: IO ()
main = do
    runTestTT $ TestList
        [
            jqFilterParseTest
        ,   jqFilterParseSpacesTest
        ,   jqQueryParserTest
        ]
    return()

jqFilterParseTest :: Test
jqFilterParseTest = TestList
    [
        "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil
    ,   "jqFilterParser test 2" ~: parseJqFilter ".[0]" ~?= Right (JqIndex 0 JqNil)
    ,   "jqFilterParser test 3" ~: parseJqFilter ".fieldName" ~?= Right (JqField "fieldName" JqNil)
    ,   "jqFilterParser test 4" ~: parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    ,   "jqFilterParser test 5" ~: parseJqFilter ".fieldName[0]" ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqFilterParseSpacesTest :: Test
jqFilterParseSpacesTest = TestList
    [
        "jqFilterParser spaces test 1" ~: parseJqFilter " . " ~?= Right JqNil
    ,   "jqFilterParser spaces test 2" ~: parseJqFilter " . [ 0 ] " ~?= Right (JqIndex 0 JqNil)
    ,   "jqFilterParser spaces test 3" ~: parseJqFilter " . fieldName " ~?= Right (JqField "fieldName" JqNil)
    ,   "jqFilterParser spaces test 4" ~: parseJqFilter " . [ 0  ] . fieldName " ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    ,   "jqFilterParser spaces test 5" ~: parseJqFilter " . fieldName [ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqQueryParserTest :: Test
jqQueryParserTest = TestList
    [
        "jqQueryParser test 1" ~: parseJqQuery "[]" ~?= Right (JqQueryArray [])
    ,   "jqQueryParser test 2" ~: parseJqQuery "[.hoge,.piyo]" ~?= Right (JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)])
    ,   "jqQueryParser test 3" ~: parseJqQuery "{\"hoge\":[], \"piyo\": []}" ~?= Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
    ]