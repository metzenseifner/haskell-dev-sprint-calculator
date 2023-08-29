{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- read https://markkarpov.com/tutorial/th.html
{-# LANGUAGE TemplateHaskell #-}

module Config_Haskell_Templates_Generate_JSON_DTOs where

-- https://mmhaskell.com/blog/2017/6/5/flexible-data-with-aeson
-- https://ro-che.info/articles/2015-07-26-better-yaml-parsing
-- for [r|raw string goes here|] ext to QuasiQuotes

import Data.Aeson.TH (deriveJSON, defaultOptions) -- aeson >=2 incompatibility issues with Data.Yaml



import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map
import Data.Maybe (fromJust)
import Data.Yaml
import qualified Data.Yaml as Y
import Text.RawString.QQ
import Data.Time

-- See https://github.com/deepin-community/haskell-yaml

configYaml :: ByteString
configYaml =
  [r|
  base:
    date: 2023-01-01
    sprint: 123
  |]

-- Note this haskell representation of data can be used to 
-- auto gen FromJSON / ToJSON types at compile time.
data Base = Base {
  base :: StartingSprint
} deriving (Show)

data StartingSprint = StartingSprint {
  date   :: String, -- Day
  sprint :: Int
} deriving (Show)

-- The two apostrophes before a type name is template haskell syntax
-- defaultOptions can be replaced with a record for more control
deriveJSON defaultOptions ''Base
deriveJSON defaultOptions ''StartingSprint

-- baseHaskellRepresentationExample :: Base
-- baseHaskellRepresentationExample = Base
--   {
--     base = StartingSprint
--       {
--         date = "2023-01-01",
--         sprint = 123
--       }
--   }


-- {
--   "baseline" : {
--     "date": "2023-01-01"
--     "sprint": 123
--   }
-- }

-- aeson lib encapsulates all JSON types in
-- data Value =
--   Object (HashMap Text Value) |
--   Array (Vector Value) |
--   String Text |
--   Number Scientific |
--   Bool Bool |
--   Null


--
extractBase :: IO Base
extractBase = do
  case Y.decodeEither' (configYaml) of
    Right base -> return base
    Left err -> error ("Failure while trying to parse input: " ++ show err)

main :: IO ()
--     pseudo-java code using fpinjava:
--     extractBase.flatMap(b -> convertToStringAndPrint)
main = extractBase >>= \b -> putStrLn $ (show b) ++ "     <-- Config_Haskell_Templates_Generate_JSON_DTOs"

