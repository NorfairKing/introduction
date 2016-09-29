Introduction
=========

[![Build Status](https://travis-ci.org/NorfairKing/introduction.svg?branch=master)](https://travis-ci.org/NorfairKing/introduction)
[![Hackage](https://img.shields.io/hackage/v/introduction.svg)](https://hackage.haskell.org/package/introduction)
[![Hackage](https://img.shields.io/hackage/v/introduction.svg)](https://hackage.haskell.org/package/introduction-test)

A prelude for *safe* new projects and another prelude for their tests.

Design points:

* Compatible with GHC 8
* No partial functions in `Introduction`
* Partial functions in `Unsafe` with compiler warnings.
* Compiler warning on unsafe functions and bottoms.
* Polymorphic string IO functions.
* Type for common data structures in scope.
* Type for all common string types in scope.
* Common monad transformers in scope by default.
* `Foldable` / `Traversable` functions in scope by default.


Usage
-----

In cabal file:

```
library
  [...]
  extensions: NoImplicitPrelude
  [...]
```

... or on a per-module basis:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Introduction
```

License
-------

Copyright (c) 2016, Tom Sydney Kerckhove
