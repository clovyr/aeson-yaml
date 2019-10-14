{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Data.Aeson.Yaml where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HashMap
import Data.String.QQ (s)
import qualified Data.Yaml

-- import Hedgehog
-- import Hedgehog.Gen
-- import Hedgehog.Gen.JSON (genJSONValue, sensibleRanges)
-- import Hedgehog.Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Data.Aeson.Yaml

data TestCase =
  TestCase
    { tcName :: String
    , tcInput :: Data.ByteString.Lazy.ByteString
    , tcOutput :: Data.ByteString.Lazy.ByteString
    , tcAlwaysQuote :: Bool
    }

testCases :: [TestCase]
testCases =
  [ TestCase
      { tcName = "Nested lists and objects"
      , tcInput =
          [s|
{
   "nullValue": null,
   "isTrue": true,
   "isFalse": false,
   "numberString": "12345",
   "quoted ! key": true,
   "ON": "ON",
   "off": "off",
   "piString": "3.14",
   "expString": "1e3",
   "apiVersion": "apps/v1",
   "kind": "Deployment",
   "metadata": {
      "labels": {
         "app": "foo"
      },
      "name": "{{ .Release.Name }}-deployment"
   },
   "spec": {
      "replicas": 1,
      "selector": {
         "matchLabels": {
            "app": "foo"
         }
      },
      "template": {
         "metadata": {
            "labels": {
               "app": "foo"
            },
            "name": "{{ .Release.Name }}-pod"
         },
         "spec": {
            "containers": [
               {
                  "command": [
                     "/data/bin/foo",
                     "--port=7654"
                  ],
                  "image": "ubuntu:latest",
                  "name": "{{ .Release.Name }}-container",
                  "ports": [
                     {
                        "containerPort": 7654
                     }
                  ],
                  "volumeMounts": [
                     {
                        "mountPath": "/data/mount1",
                        "name": "{{ .Release.Name }}-volume-mount1"
                     },
                     {
                        "mountPath": "/data/mount2",
                        "name": "{{ .Release.Name }}-volume-mount2"
                     }
                  ]
               }
            ]
         }
      }
   }
}
|]
      , tcOutput =
          [s|"ON": "ON"
apiVersion: apps/v1
expString: "1e3"
isFalse: false
isTrue: true
kind: Deployment
metadata:
  labels:
    app: foo
  name: "{{ .Release.Name }}-deployment"
nullValue: null
numberString: "12345"
"off": "off"
piString: "3.14"
"quoted ! key": true
spec:
  replicas: 1
  selector:
    matchLabels:
      app: foo
  template:
    metadata:
      labels:
        app: foo
      name: "{{ .Release.Name }}-pod"
    spec:
      containers:
        - command:
            - /data/bin/foo
            - "--port=7654"
          image: ubuntu:latest
          name: "{{ .Release.Name }}-container"
          ports:
            - containerPort: 7654
          volumeMounts:
            - mountPath: /data/mount1
              name: "{{ .Release.Name }}-volume-mount1"
            - mountPath: /data/mount2
              name: "{{ .Release.Name }}-volume-mount2"|]
      , tcAlwaysQuote = False
      }
  , TestCase
      { tcName = "Quoted"
      , tcInput = [s|{"foo": "bar", "baz": "quux"}|]
      , tcOutput =
          [s|"baz": "quux"
"foo": "bar"|]
      , tcAlwaysQuote = True
      }
  ]

foo :: Data.Aeson.Value
foo = Data.Aeson.Object $ HashMap.fromList [("foo", "bar")]

test_testCases :: TestTree
test_testCases = testGroup "Test Cases" $ map mkTestCase testCases
  where
    mkTestCase TestCase {..} =
      testCase tcName $ do
        assertEqual "Expected output" tcOutput output
        assertEqual
          "libyaml decodes the original value"
          decodedInput
          decodedYaml
        if tcAlwaysQuote
          then assertEqual
                 "Expected documents output"
                 ("\"foo\": \"bar\"" <> "\n---\n" <> output)
                 (encodeQuotedDocuments [foo, decodedInput])
          else assertEqual
                 "Expected documents output"
                 ("foo: bar" <> "\n---\n" <> output)
                 (encodeDocuments [foo, decodedInput])
      where
        output =
          (if tcAlwaysQuote
             then encodeQuoted
             else encode)
            decodedInput
        decodedInput :: Data.Aeson.Value
        decodedInput =
          fromRight (error "couldn't decode JSON") $
          Data.Aeson.eitherDecode' tcInput
        decodedYaml :: Data.Aeson.Value
        decodedYaml =
          fromRight (error "couldn't decode YAML") $
          Data.Yaml.decodeEither' (Data.ByteString.Lazy.toStrict output)
-- TODO: SBV dep of hedgehog-gen-json doesn't currently build
-- hprop_decodesSameAsLibyaml :: Property
-- hprop_decodesSameAsLibyaml =
--   property $ do
--     v <- forAll $ genJSONValue sensibleRanges
--     let enc1 = Data.Aeson.Yaml.encode v
--         enc2 = Data.Yaml.encode v
--         dec1 =
--           fromRight
--             (error "couldn't decode Aeson.Yaml-encoded value")
--             (Data.Yaml.decodeEither' enc1)
--         dec2 =
--           fromRight
--             (error "couldn't decode Data.Yaml-encoded value")
--             (Data.Yaml.decodeEither' enc2)
--     dec1 === dec2
