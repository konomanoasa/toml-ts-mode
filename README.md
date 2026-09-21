# toml-ts-mode

[![CI](https://github.com/konomanoasa/toml-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/toml-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
Tom's Obvious, Minimal Language (TOML) 1.1.0.

## Requirement

- Emacs 31.1 or later

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/toml-ts-mode")
```

## Automatic Activation

Enabled for `.toml` files and `Cargo.lock`.

## Features

- Comment Commands
- Font Lock
- Imenu: table headers
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock |
| --- | --- |
| 1 | Comments |
| 2 | Keys and strings |
| 3 | Numbers, booleans, dates and times, and escapes |
| 4 | Operators, delimiters, and brackets |

## Grammar

[tree-sitter-toml](https://github.com/konomanoasa/tree-sitter-toml)

## License

[MIT](LICENSE)
