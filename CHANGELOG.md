# Changelog

## 1.1.0.1 - 2021-11-13

### Changed

  - Support aeson-2.0 (thanks to Simon Jakobi)

## 1.1.0.0 - 2020-06-25

### Changed

  - Every change to the YAML output format will now result in a bump of B
    in A.B.C.D to more closely follow the Haskell PVP. B will be bumped in
    case of small changes and bug fixes, and A if a change is expected to
    cause problems with common YAML 1.1 or 1.2 decoders.
  - `encodeDocuments` and `encodeQuotedDocuments` now output a leading `---`

## 1.0.6.0 - 2020-02-27

### Changed

  - Don't quote simple strings containing spaces, e.g: hello world

  - Single-quote dates (like '2020-02-27') and bools ('true' / 'false')

## 1.0.5.0 - 2019-11-30

### Fixed

  - Encode empty objects as "{}" rather than ""

## 1.0.4.0 - 2019-11-06

### Fixed

  - Quote date strings (e.g. "2038-01-19")

## 1.0.3.0 - 2019-11-03

### Fixed

  - Encode empty lists as "[]" rather than "\n -"

## 1.0.2.0 - 2019-10-15

### Changed

  - Only quote YAML 1.2 boolean strings "true" and "false" (upper or
    lowercase), not "on", "off", "yes", "y", "no", "n" (if you want to quote
    these strings, use `encodeQuoted`).

## 1.0.1.0 - 2019-10-15

### Added

  - `encodeQuoted`: Encodes with all keys/strings quoted
  - `encodeQuotedDocuments`: Encodes documents with all keys/strings quoted

### Changed

  - Simple strings (scalars) are now written unquoted.

  - Multi-line strings (with trailing newlines) are now written as literal
    block scalars.


## 1.0.0.0 - 2019-09-30

Initial version
