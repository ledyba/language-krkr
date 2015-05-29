{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec where

import Test.Hspec
import Parser.Parser (parse)
import Parser.Tree
import           Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Error(errorMessages)

instance Eq ParseError where
    a == b = errorMessages a == errorMessages b

spec :: Spec
spec = do
  describe "num literal test" $ do
    it "Parse Dec Int" $ parse "123" `shouldBe` Right (Int 123)
    it "Parse Dec Int" $ parse "123e2" `shouldBe` Right (Int 12300)
    it "Parse Dec Int" $ parse "123.0e2" `shouldBe` Right (Real 12300.0)
  describe "str literal test" $ do
    it "Parse Single Str" $ parse "'anata to java'" `shouldBe` Right (Str "anata to java")
    it "Parse Double Str" $ parse "\"anata to java\"" `shouldBe` Right (Str "anata to java")
