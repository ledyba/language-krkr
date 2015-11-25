# TJS Parser for Haskell

 TJS is a language used in [Kirikiri](https://github.com/krkrz/krkrz), a Japanese "bishojo" Game Engine.

 This library implements a parser for this language.

## 0. Prerequisites

 You need a latest Haskell Platform:

  - https://www.haskell.org/platform/

## 1.install

```bash
git clone git@github.com:ledyba/language-tjs.git
cd language-tjs
cabal install
```

## 2.How to Use

import my library,

```haskell
import Language.TJS as TJS
```

then,

```
TJS.parse "filename" "source"
```
