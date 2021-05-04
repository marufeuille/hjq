module Data.Hjq.Parser (
    JqFilter(JqField, JqIndex, JqNil)
) where
data JqFilter
    = JqField String JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

parseJqFilter :: String -> Either String JqFilter
parseJqFilter s = undefined