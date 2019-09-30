# aeson-yaml

BSD3-licensed library to encode any Aeson value as YAML, without a
dependency on an external YAML library like `yaml` (libyaml C FFI) or
`HsYaml` (GPL).

## Usage

```haskell
import qualified Data.Aeson.Yaml as Aeson.Yaml

Aeson.Yaml.encode :: ToJSON a => a -> LazyByteString

-- To encode multiple values, separated by '---' (YAML documents),
-- use `encodeDocuments`.
Aeson.Yaml.encodeDocuments :: ToJSON a => [a] -> LazyByteString

-- To encode values of different types, use `toJSON` from `Data.Aeson`
-- like so:
encodeDocuments [toJSON x, toJSON y, toJSON z]
```

See [bin/JsonToYaml.hs](bin/JsonToYaml.hs) for a simple command-line application
using this library.

## Documentation

[Hackage](https://hackage.haskell.org/package/aeson-yaml)

## License

[BSD3](LICENSE)
