{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- Generic is a typeclass that allows GHC to represent your types at a level of generic constructors.
-- In other words, we do not need to manually define the FromJSON objects like we
-- did above if we add derive (Generic, ToJson, FromJSON) to the data records.
-- {-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Config_Generics_Deriving_JSON_DTOs where

-- https://mmhaskell.com/blog/2017/6/5/flexible-data-with-aeson
-- https://ro-che.info/articles/2015-07-26-better-yaml-parsing
-- for [r|raw string goes here|] ext to QuasiQuotes
import GHC.Generics

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
data Config = Config {
  base :: StartingSprint
} deriving (Show, Generic, ToJSON, FromJSON)

data StartingSprint = StartingSprint {
  date   :: String, -- Day
  sprint :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

-- baseHaskellRepresentationExample :: Config
-- baseHaskellRepresentationExample = Config
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
extractConfig :: IO Config
extractConfig = do
  case Y.decodeEither' (configYaml) of
    Right base -> return base
    Left err -> error ("Failure while trying to parse input: " ++ show err)

main :: IO ()
--     pseudo-java code using fpinjava:
--     extractConfig.flatMap(b -> convertToStringAndPrint)
main = extractConfig >>= \b -> putStrLn $ (show b) ++ "     <-- Config_Generics_Deriving_JSON_DTOs"

