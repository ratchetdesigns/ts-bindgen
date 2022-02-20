# Change Log

## [0.4.0] - 2022-02-20

### BREAKING CHANGE

- generate typescript union variant names without a "Case" suffix

### Fixed

- fix clippy issues with generated code

## [0.3.0] - 2022-02-19

### BREAKING CHANGE

- js-sys types are now generated as the proper js-sys type instead of as JsValue. #3
- overloaded functions and methods now have a _FnXToY suffix #4

### Added

- support overloaded functions and methods. #4
- generate js-sys types for corresponding typescript types. #3
- support [typescript utility types](https://www.typescriptlang.org/docs/handbook/utility-types.html) #6

### Fixed

- fix array serialization

## [0.2.0] - 2022-02-06

### BREAKING CHANGE

- web-sys types are now generated as the proper web-sys type instead of as JsValue. #1

### Added

- recognize and use corresponding web-sys builtins for types. #1

### Fixed

- make type flattening recursive. #2

## [0.1.0] - 2022-01-30

- initial release
