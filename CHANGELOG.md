# Changelog

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
