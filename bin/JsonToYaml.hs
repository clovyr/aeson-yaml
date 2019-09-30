module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Yaml as Aeson.Yaml
import qualified Data.ByteString.Lazy as ByteString.Lazy

main :: IO ()
main =
  ByteString.Lazy.interact $ \s ->
    case Aeson.eitherDecode' s of
      Left err -> error ("Failed to decode JSON: " <> err)
      Right v -> Aeson.Yaml.encode (v :: Aeson.Value)
