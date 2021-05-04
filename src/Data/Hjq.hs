module Data.Hjq (
    parseJqFilter
) where
import Data.Hjq.Parser (JqFilter(JqField, JqIndex, JqNil))

parseJqFilter :: String -> Either String JqFilter
parseJqFilter _ = Right JqNil