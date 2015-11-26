module Language.KAG.Tree (
  Kag
  -- module Language.TJS.Tree
  ,SrcSpan(..)
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

data Kag =
    KagText Text
  | KagTag String [(String,Maybe Text)]
  | KagLabel String (Maybe Text)
  deriving (Ord,Eq,Show)
