{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParsingLibrarySpec where

import Prelude hiding (readFile)
import Test.Hspec
import qualified Language.TJS.Parser as P
import Language.TJS.Tree
import Text.ParserCombinators.Parsec.Error (ParseError)
import Data.Text hiding (filter,concat)
import Control.Monad
import System.Directory
import Data.String.Utils
import System.FilePath (pathSeparator)
import Data.ByteString (readFile)
import Data.ByteString.Char8 ()
import Data.Text.ICU.Convert (open,toUnicode)

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

--instance Eq ParseError where
--    a == b = errorMessages a == errorMessages b

parse :: FilePath -> Text -> Either ParseError Stmt
parse = P.parse

enumAllTjs :: FilePath -> IO [FilePath]
enumAllTjs path = do
    allItems <- getDirectoryContents path
    let items = fmap (\f -> path ++ pathSeparator:f) $ filter (\k -> k /= "." && k /= "..") allItems
    justFiles <- filterM doesFileExist items
    justFolders <- filterM doesDirectoryExist items
    let srcs = filter (endswith ".tjs") justFiles
    leftFiles <- mapM enumAllTjs justFolders
    return (srcs ++ concat leftFiles)

parseTest :: FilePath -> String -> Spec
parseTest dir code = do
  files <- runIO $ enumAllTjs dir
  conv <- runIO $ open code (Just False)
  mapM_ (\f -> do
      src <- runIO $ readFile f
      let asrc = toUnicode conv src
      it ("Parse: " ++ f) $ parse f asrc `shouldSatisfy` isRight
    ) files

spec :: Spec
spec = do
  describe "KTL Parse Test" $ parseTest "sample/ktl" "utf16"
  describe "Sample Parse Test" $ parseTest "sample/krkr2sample" "utf8"
