module ParserSpec where

import Test.Hspec
import Parser.Parser (parse)
import Parser.Tree

spec :: Spec
spec = do
  describe "num literal test" $ do
    it "Parse Dec Int" $ parse "123" `shouldBe` Right (Int 123)
    it "Parse Dec Int" $ parse "123e2" `shouldBe` Right (Int 12300)
    it "Parse Dec Int" $ parse "123.0e2" `shouldBe` Right (Real 12300.0)
