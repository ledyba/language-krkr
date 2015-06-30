{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec where

import Test.Hspec
import Parser.Parser (parse)
import Parser.Tree
import Text.ParserCombinators.Parsec.Error()

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

--instance Eq ParseError where
--    a == b = errorMessages a == errorMessages b

spec :: Spec
spec = do
  describe "num literal test" $ do
    it "Parse Dec Int" $ parse "123" `shouldBe` Right (Expr $ Int 123)
    it "Parse Dec Int" $ parse "123e2" `shouldBe` Right (Expr $ Int 12300)
    it "Parse Dec Int" $ parse "123.0e2" `shouldBe` Right (Expr $ Real 12300.0)
  describe "str literal test" $ do
    it "Parse Single Str" $ parse "'anata to java'" `shouldBe` Right (Expr $ Str "anata to java")
    it "Parse Double Str" $ parse "\"anata to java\"" `shouldBe` Right (Expr $ Str "anata to java")
    it "Parse Double Str" $ parse "\"anata \\\"to java\"" `shouldBe` Right (Expr $ Str "anata \"to java")
    it "Parse Double Str" $ parse "\"anata 'to java\"" `shouldBe` Right (Expr $ Str "anata 'to java")
    it "Parse Double Str" $ parse "\"anata \\n\\nto java\"" `shouldBe` Right (Expr $ Str "anata \n\nto java")
  describe "identifer test" $ do
    it "Parse Identifer" $ parse "abc123" `shouldBe` Right (Expr $ Ident $ Identifer "abc123")
    it "Parse Identifer" $ parse "__abc123__" `shouldBe` Right (Expr $ Ident $ Identifer "__abc123__")
    it "Parse Identifer" $ parse "1__abc123__" `shouldSatisfy` isLeft
