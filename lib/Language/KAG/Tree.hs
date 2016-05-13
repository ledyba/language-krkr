module Language.KAG.Tree (
    Kag(..)
  , KagValue(..)
  -- module Language.TJS.Tree
  , SrcSpan(..)
  , SourcePos
  , SourceName, Line, Column
  , sourceName, sourceLine, sourceColumn
) where

import Data.Text
import Language.TJS.Tree (
   SrcSpan(..)
  ,SourcePos
  ,SourceName, Line, Column
  ,sourceName, sourceLine, sourceColumn)
import qualified Language.TJS.Tree as TJS

data Kag =
    KagText Text SrcSpan
  | KagTag Text [(Text,Maybe KagValue)] SrcSpan
  | KagLabel Text (Maybe Text) SrcSpan
  | KagNewline SrcSpan
  deriving (Eq,Show)

data KagValue =
    KagStrValue Text SrcSpan
  | KagTjsValue TJS.Stmt SrcSpan
  | KagLabelValue Text SrcSpan
  deriving (Eq,Show)
