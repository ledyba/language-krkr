{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module KagParserSpec where

import Prelude hiding (readFile)
import qualified Language.KAG.Parser as P
import Language.KAG.Tree
import Text.ParserCombinators.Parsec.Error (ParseError)
import Data.Text hiding (filter,concat)
import Control.Monad
import Test.Hspec
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

parse :: FilePath -> Text -> Either ParseError [Kag]
parse = P.parse

enumAllKags :: FilePath -> IO [FilePath]
enumAllKags path = do
    allItems <- getDirectoryContents path
    let items = (\f -> path ++ pathSeparator:f) <$> filter (\k -> k /= "." && k /= "..") allItems
    justFiles <- filterM doesFileExist items
    justFolders <- filterM doesDirectoryExist items
    let srcs = filter (endswith ".ks") justFiles
    leftFiles <- mapM enumAllKags justFolders
    return (srcs ++ concat leftFiles)

spec :: Spec
spec =
  describe "Kag Parse Test" $ do
    files <- runIO (enumAllKags "sample/kag")
    conv <- runIO $ open "utf8" (Just False)
    mapM_ (\f -> do
        src <- runIO $ readFile f
        let asrc = toUnicode conv src
        it ("Parse: " ++ f) $ parse f asrc `shouldSatisfy` isRight
      ) files
