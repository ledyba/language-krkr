{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module KTLTestSpec where

import Prelude hiding (readFile)
import Test.Hspec
import qualified Language.TJS.Parser as P
import Language.TJS.Tree
import Text.ParserCombinators.Parsec.Error (ParseError)
import Data.Text hiding (filter,concat)
import Control.Monad.IO.Class
import Control.Monad
import Data.List
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
    let srcs = filter (\k -> not (k `endswith` ".tjs")) justFiles
    leftFiles <- mapM enumAllTjs (filter (\k -> not(k `startswith` ".")) justFolders)
    return (srcs ++ concat leftFiles)

spec :: Spec
spec =
  describe "num literal test" $ do
    files <- runIO (enumAllTjs "sample/ktl")
    conv <- runIO $ open "utf16" (Just False)
    mapM_ (\f -> do
        src <- runIO $ readFile f
        let asrc = toUnicode conv src
        it ("Parse: " ++ f) $ parse f asrc `shouldSatisfy` isRight
      ) files
