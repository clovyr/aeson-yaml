# aeson-yaml

BSD3-licensed, pure Haskell library to encode any Aeson value as YAML.

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

## Motivation

This library does not depend on any external YAML library with C bindings,
like `yaml`, or a restrictive license, like `HsYaml` (GPLv3). Note, though,
that this library can only be used for encoding, not decoding.

This library also works with GHCJS and Eta.
