# Krkr Parser for Haskell

 [Krkr](https://github.com/krkrz/krkrz) is a Japanese "bishojo" Game Engine.

 To create Krkr game, you can use 2 languages, TJS and KAG:

 - TJS: JavaScript like language.
 - KAG: scenario scripts.

This library only provides a TJS parser now. Implementing a KAG parser is also planned for future, of course.

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
import Language.TJS as TJS
```

then, call parse function:

```haskell
TJS.parse "filename" "source"
```

## 3.References

 - TJS2 Manual: http://devdoc.kikyou.info/tvp/docs/tjs2doc/contents/
