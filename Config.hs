{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Config where

-- https://ro-che.info/articles/2015-07-26-better-yaml-parsing
-- for [r|raw string goes here|] ext to QuasiQuotes

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map
import Data.Maybe (fromJust)
import Data.Yaml
import qualified Data.Yaml as Y
import Text.RawString.QQ

-- See https://github.com/deepin-community/haskell-yaml

configYaml :: ByteString
configYaml =
  [r|
baseline:
  date: 2023-01-01
  sprint: 123
|]

data SprintBase = SprintBase
  { date :: String,
    sprint :: Int
  }
  deriving (Show, Eq)

newtype Baseline = Baseline {baseline :: SprintBase}

-- data Baseline = Baseline
--  { baseline :: SprintBase
--  }
--  deriving (Eq, Show)

-- instance FromJSON Baseline where
--  parseJSON = withObject "Baseline" $ o -> Baseline <$>
--    o .: "baseline"
--    <*> o .: "age"

-- Could use Generic deriving, but doing by hand here:
instance FromJSON Baseline where
  parseJSON (Object o) = Baseline <$> (o .: "baseline")
  parseJSON _ = error "Can't parse Task from YAML/JSON"

instance ToJSON Baseline where
  toJSON (Baseline c) = "baseline" .= c

type ErrorMsg = String

readMyType :: ByteString -> Either ErrorMsg Baseline
readMyType yamlObj = do
  parsedContent <- decodeEither yamlObj

  -- mapOfStrings <- parseEither (.: "baseline") yamlObj
  case parsedContent of
    Right yamlObj' -> Right yamlObj'
    Left exception -> Left $ show (exception)

-- instance FromJSON Baseline where
--   parseJSON (Y.Object v) = -- called parseJSON because it derives from the aeson lib
--     Baseline <$>
--       v .: "baseline"
--       <*> Baseline <$>
--         v .: "date" <*>
--         v .: "sprint"
--   parseJSON _ = error "Cannot parse 'baseline' from YAML/JSON"
--
-- Cool wrapper I found https://cthulehansen.github.io/blog/posts/jsonyaml.html
yamlDecode :: (FromJSON a) => ByteString -> Either String a
yamlDecode a = case Data.Yaml.decodeEither' a of
  Left pe -> Left $ show pe
  Right a' -> Right a'

main = do
  -- configYaml <- BS.readFile "config.yml"
  -- let config = Data.Yaml.decode configYaml :: Maybe [Baseline]
  -- config <- Data.Yaml.decodeThrow configYaml
  -- parsedConfig <- readMyType (configYaml)
  case yamlDecode configYaml of
    Right d -> putStrLn d
    Left e -> putStrLn e
