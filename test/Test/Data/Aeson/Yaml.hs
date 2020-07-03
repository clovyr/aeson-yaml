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
  [tcDataTypes, tcNestedListsAndObjects, tcQuoted, tcNetworkQuote, tcEmptyList, tcHelloWorld]

tcEmptyList :: TestCase
tcEmptyList =
  TestCase
    { tcName = "Empty root list"
    , tcInput = "[]"
    , tcOutput = "[]\n"
    , tcAlwaysQuote = False
    }

tcDataTypes :: TestCase
tcDataTypes =
  TestCase
    { tcName = "Data types"
    , tcInput =
        [s|
{
   "nullValue": null,
   "emptyObject": {},
   "isTrue": true,
   "isFalse": false,
   "numberString": "12345",
   "quoted ! key": true,
   "boolString": "true",
   "dateString": "2038-01-19",
   "piString": "3.14",
   "expString": "1e3",
   "leadingSpace" : " leading space",
   "leadingSymbol" : "!leading symbol",
   "asteriskString": "*",
   "multiLine": "The first line is followed by the\nsecond line\n",
   "multiLineWithSpaces": "         This has extra\n     spaces at the beginning\n",
   "notMultiline": "This won't be\nmulti-lined",
   "list": ["foo", "bar", "baz"],
   "listEmpty": []
}
|]
    , tcOutput =
        [s|asteriskString: "*"
boolString: 'true'
dateString: '2038-01-19'
emptyObject: {}
expString: '1e3'
isFalse: false
isTrue: true
leadingSpace: " leading space"
leadingSymbol: "!leading symbol"
list:
  - foo
  - bar
  - baz
listEmpty: []
multiLine: |
  The first line is followed by the
  second line
multiLineWithSpaces: |2
           This has extra
       spaces at the beginning
notMultiline: "This won't be\nmulti-lined"
nullValue: null
numberString: '12345'
piString: '3.14'
"quoted ! key": true
|]
    , tcAlwaysQuote = False
    }

tcNestedListsAndObjects :: TestCase
tcNestedListsAndObjects =
  TestCase
    { tcName = "Nested lists and objects"
    , tcInput =
        [s|
{
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
                     "/42",
                     "--port=7654"
                  ],
                  "image": "ubuntu:latest",
                  "name": "{{ .Release.Name }}-container",
                  "ports": [
                     {
                        "containerPort": 7654
                     }
                  ],
                  "script": "#!/bin/bash\necho hello world\n",
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
        [s|apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: foo
  name: "{{ .Release.Name }}-deployment"
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
            - /42
            - "--port=7654"
          image: ubuntu:latest
          name: "{{ .Release.Name }}-container"
          ports:
            - containerPort: 7654
          script: |
            #!/bin/bash
            echo hello world
          volumeMounts:
            - mountPath: /data/mount1
              name: "{{ .Release.Name }}-volume-mount1"
            - mountPath: /data/mount2
              name: "{{ .Release.Name }}-volume-mount2"
|]
    , tcAlwaysQuote = False
    }

tcQuoted :: TestCase
tcQuoted =
  TestCase
    { tcName = "Quoted"
    , tcInput = [s|{"foo": "bar", "baz": "quux"}|]
    , tcOutput =
        [s|'baz': 'quux'
'foo': 'bar'
|]
    , tcAlwaysQuote = True
    }

tcNetworkQuote :: TestCase
tcNetworkQuote =
  TestCase
    { tcName = "Network CIDR single quote"
    , tcInput =
        [s|
{ "ip": "127.0.0.1"
, "net": "127.0.0.1/8"
}
|]
    , tcOutput =
        [s|ip: '127.0.0.1'
net: '127.0.0.1/8'
|]
    , tcAlwaysQuote = False
    }


tcHelloWorld :: TestCase
tcHelloWorld =
  TestCase
    { tcName = "Hello World"
    , tcInput =
        [s|
{ "image": "node 10.15.3"
, "pipelines":
  { "default":
    [ { "step":
        { "caches": []
        , "name": "hello world"
        , "script": [ "echo hello" ]
        }
      }
    ]

  }
}
|]
    , tcOutput =
        [s|image: node 10.15.3
pipelines:
  default:
    - step:
        caches: []
        name: hello world
        script:
          - echo hello
|]
    , tcAlwaysQuote = False
    }

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
                 ("---\n'foo': 'bar'" <> "\n---\n" <> output)
                 (encodeQuotedDocuments [foo, decodedInput])
          else assertEqual
                 "Expected documents output"
                 ("---\nfoo: bar" <> "\n---\n" <> output)
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
