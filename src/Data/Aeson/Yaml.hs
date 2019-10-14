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
  , encodeQuoted
  , encodeQuotedDocuments
  ) where

import Data.Aeson hiding (encode)
import qualified Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Short as ByteString.Short
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intersperse, sortOn)
import Data.Monoid ((<>), mconcat, mempty)
import qualified Data.Text as Text
import Data.Text (Text)
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
encode =
  ByteString.Builder.toLazyByteString . encodeBuilder False False 0 . toJSON

-- | Encode multiple values separated by '---'. To encode values of different
-- types, @import Data.Aeson(ToJSON(toJSON))@ and do
-- @encodeDocuments [toJSON x, toJSON y, toJSON z]@.
encodeDocuments :: ToJSON a => [a] -> ByteString.Lazy.ByteString
encodeDocuments =
  ByteString.Builder.toLazyByteString .
  mconcat .
  intersperse (bs "\n---\n") . map ((encodeBuilder False False 0) . toJSON)

-- | Encode a value as YAML (lazy bytestring). Strings (scalars) are always
-- quoted.
encodeQuoted :: ToJSON a => a -> ByteString.Lazy.ByteString
encodeQuoted =
  ByteString.Builder.toLazyByteString . encodeBuilder True False 0 . toJSON

-- | Encode multiple values separated by '---'. Strings (scalars) are always
-- quoted.
encodeQuotedDocuments :: ToJSON a => [a] -> ByteString.Lazy.ByteString
encodeQuotedDocuments =
  ByteString.Builder.toLazyByteString .
  mconcat .
  intersperse (bs "\n---\n") . map ((encodeBuilder True False 0) . toJSON)

encodeBuilder :: Bool -> Bool -> Int -> Data.Aeson.Value -> Builder
encodeBuilder alwaysQuote newlineBeforeObject level value =
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
      map (encodeBuilder alwaysQuote False (level + 1)) (Vector.toList vec)
      where prefix = bs "\n" <> indent level <> bs "- "
    String s -> encodeText alwaysQuote s
    Number n -> bl (enc n)
    Bool bool -> bl (enc bool)
    Null -> bs "null"
  where
    keyValue level' (k, v) =
      mconcat
        [ encodeText alwaysQuote k
        , ":"
        , case v of
            Object _ -> ""
            Array _ -> ""
            _ -> " "
        , encodeBuilder alwaysQuote True (level' + 1) v
        ]

encodeText :: Bool -> Text -> Builder
encodeText alwaysQuote s
  | alwaysQuote || not unquotable = bl $ enc s
  | otherwise = b (Text.Encoding.encodeUtf8 s)
  where
    unquotable =
      s /= "" &&
      (not $ isSpecial s) &&
      isSafeAscii (Text.head s) &&
      not (Text.all isDigit s) && Text.all isAllowed s
    isSpecial s'
      | Text.length s > 5 = False
      | otherwise =
        case Text.toLower s' of
          "true" -> True
          "false" -> True
          "on" -> True
          "off" -> True
          "y" -> True
          "yes" -> True
          "n" -> True
          "no" -> True
          _ -> False
    isSafeAscii c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') || c == '/' || c == '_' || c == '.' || c == '='
    isAllowed c
      -- We don't include ' ' here to avoid sequences like " -" and ": "
     = isSafeAscii c || c == '-' || c == ':'
