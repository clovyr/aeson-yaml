{-|
This library exposes functions for encoding any Aeson value as YAML.
There is also support for encoding multiple values into YAML
"documents".

This library is pure Haskell, and does not depend on C FFI with
libyaml. It is also licensed under the BSD3 license.

This module is meant to be imported qualified.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Yaml
  ( encode
  , encodeDocuments
  ) where

import Data.Aeson hiding (encode)
import qualified Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Short as ByteString.Short
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn)
import Data.List (intersperse)
import Data.Monoid ((<>), mconcat, mempty)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector

b :: ByteString -> Builder
b = ByteString.Builder.byteString

bl :: ByteString.Lazy.ByteString -> Builder
bl = ByteString.Builder.lazyByteString

bs :: ByteString.Short.ShortByteString -> Builder
bs = ByteString.Builder.shortByteString

indent :: Int -> Builder
indent 0 = mempty
indent n = bs "  " <> (indent $! n - 1)

enc :: ToJSON a => a -> ByteString.Lazy.ByteString
enc = Data.Aeson.encode

-- | Encode a value as YAML (lazy bytestring).
encode :: ToJSON a => a -> ByteString.Lazy.ByteString
encode = ByteString.Builder.toLazyByteString . encodeBuilder False 0 . toJSON

-- | Encode multiple values separated by '---'. To encode values of different
-- types, @import Data.Aeson(ToJSON(toJSON))@ and do
-- @encodeDocuments [toJSON x, toJSON y, toJSON z]@.
encodeDocuments :: ToJSON a => [a] -> ByteString.Lazy.ByteString
encodeDocuments =
  ByteString.Builder.toLazyByteString .
  mconcat . intersperse (bs "\n---\n") . map ((encodeBuilder False 0) . toJSON)

encodeBuilder :: Bool -> Int -> Data.Aeson.Value -> Builder
encodeBuilder newlineBeforeObject level value =
  case value of
    Object hm ->
      mconcat $
      (if newlineBeforeObject
         then (prefix :)
         else id) $
      intersperse prefix $ map (keyValue level) (sortOn fst $ HashMap.toList hm)
      where prefix = bs "\n" <> indent level
    Array vec ->
      mconcat $
      (prefix :) $
      intersperse prefix $
      map (encodeBuilder False (level + 1)) (Vector.toList vec)
      where prefix = bs "\n" <> indent level <> bs "- "
    String s -> bl (enc s)
    Number n -> bl (enc n)
    Bool bool -> bl (enc bool)
    Null -> bs "null"
  where
    keyValue level' (k, v) =
      mconcat
        [ b (Text.Encoding.encodeUtf8 k)
        , ":"
        , case v of
            Object _ -> ""
            Array _ -> ""
            _ -> " "
        , encodeBuilder True (level' + 1) v
        ]
