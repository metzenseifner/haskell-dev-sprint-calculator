module Main where

import qualified Config_With_Manual_JSON_DTOs
import qualified Config_Generics_Deriving_JSON_DTOs

main :: IO ()
main = do
  --Config.main
  Config_With_Manual_JSON_DTOs.main
  Config_Generics_Deriving_JSON_DTOs.main
