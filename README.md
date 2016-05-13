# Krkr Parser for Haskell

 [Krkr](https://github.com/krkrz/krkrz) is a Japanese "bishojo" Game Engine.

 To create Krkr game, you can use 2 languages, TJS and KAG:

 - TJS: JavaScript like language.
 - KAG: scenario scripts.

This library provides parsers for those languages.

## 0. Prerequisites

 You need a latest Haskell Platform:

  - https://www.haskell.org/platform/

## 1.Install

```bash
git clone git@github.com:ledyba/language-krkr.git
cd language-krkr
cabal install
```

## 2.How to Use

import this library,

```haskell
import qualified Language.TJS as TJS
import qualified Language.KAG as KAG
```

then, call parse function:

```haskell
TJS.parse "filename" "source"
KAG.parse "filename" "source"
```

## 3.References

 - TJS2 Manual: http://devdoc.kikyou.info/tvp/docs/tjs2doc/contents/
