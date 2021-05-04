{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Parser (
    JqFilter(JqField, JqIndex, JqNil),
    JqQuery(JqQueryObject, JqQueryArray, JqQueryFilter),
    jqFilterParser,
    parseJqQuery,
    parseJqFilter,
    showParseResult
) where

import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text.IO

data JqFilter
    = JqField T.Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
    where
        jqFilter :: Parser JqFilter
        jqFilter = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

        jqField :: Parser JqFilter
        jqField = JqField <$> (word <* skipSpace) <*> jqFilter

        jqIndex :: Parser JqFilter
        jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

showParseResult :: Show a => Result a -> Either T.Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . T.pack $ show r

word :: Parser T.Text
word = fmap T.pack $ many1 (letter <|> char '-' <|> char '_' <|> digit)

parseJqFilter :: T.Text -> Either T.Text JqFilter
parseJqFilter s = showParseResult
    $ parse (jqFilterParser <* endOfInput) s `feed` ""

data JqQuery
    = JqQueryObject [(T.Text, JqQuery)]
    | JqQueryArray [JqQuery]
    | JqQueryFilter JqFilter
    deriving (Show, Read, Eq)

parseJqQuery :: T.Text -> Either T.Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` ""

jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryFilter <|> queryObject where
    queryArray :: Parser JqQuery
    queryArray = JqQueryArray <$> (schar '[' *> jqQueryParser `sepBy` (schar ',') <* schar ']')

    queryObject :: Parser JqQuery
    queryObject = JqQueryObject <$> (schar '{' *> (qObj `sepBy` schar ',') <* schar '}')

    qObj :: Parser (T.Text, JqQuery)
    qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

    queryFilter :: Parser JqQuery
    queryFilter = JqQueryFilter <$> jqFilterParser