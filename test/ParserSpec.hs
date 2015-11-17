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
    it "Parse Dec Int With Comment" $ parse "123 //here is the comment" `shouldBe` Right (Expr $ Int 123)
    it "Parse Dec Int" $ parse "123e2" `shouldBe` Right (Expr $ Int 12300)
    it "Parse Dec Int" $ parse "123.0e2" `shouldBe` Right (Expr $ Real 12300.0)
    it "Parse Oct Int" $ parse "0123" `shouldBe` Right (Expr $ Int 0o123)
    it "Parse Hex Int" $ do
      parse "0x123" `shouldBe` Right (Expr $ Int 0x123)
      parse "0X123" `shouldBe` Right (Expr $ Int 0x123)
  describe "str literal test" $ do
    it "Parse Single Str" $ parse "'anata to java'" `shouldBe` Right (Expr $ Str "anata to java")
    it "Parse Single Str With Comment" $ parse "'anata to java' /* test */" `shouldBe` Right (Expr $ Str "anata to java")
    it "Parse Double Str" $ parse "\"anata to java\"" `shouldBe` Right (Expr $ Str "anata to java")
    it "Parse Double Str" $ parse "\"anata \\\"to java\"" `shouldBe` Right (Expr $ Str "anata \"to java")
    it "Parse Double Str" $ parse "\"anata 'to java\"" `shouldBe` Right (Expr $ Str "anata 'to java")
    it "Parse Double Str" $ parse "\"anata \\n\\nto java\"" `shouldBe` Right (Expr $ Str "anata \n\nto java")
  describe "identifer test" $ do
    it "Parse Identifer" $ parse "abc123" `shouldBe` Right (Expr $ Ident $ Identifer "abc123")
    it "Parse Identifer" $ parse "__abc123__" `shouldBe` Right (Expr $ Ident $ Identifer "__abc123__")
    it "Parse Identifer" $ parse "1__abc123__" `shouldSatisfy` isLeft

  describe "postop" $ do
    it "Parse PostOp" $ parse "a[123]" `shouldBe` Right (Expr (Index (Ident (Identifer "a")) (Int 123)))
    it "Parse PostOp" $ parse "a[123].test" `shouldBe` Right (Expr (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")))
    it "Parse PostOp" $ parse "a[123]++--!" `shouldBe` Right (Expr (PostUni (PostUni (PostUni (Index (Ident (Identifer "a")) (Int 123)) "++") "--") "!"))
    it "Parse PostOp" $ parse "a[123].test++--!" `shouldBe` Right (Expr (PostUni (PostUni (PostUni (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")) "++") "--") "!"))

    describe "preop" $ do
      it "Parse PreOp" $ parse "++a[123]" `shouldBe` Right (Expr (PreUni "++" (Index (Ident (Identifer "a")) (Int 123))))
      it "Parse PreOp" $ parse "++ invalidate a[123].test" `shouldBe` Right (Expr (PreUni "++" (PreUni "invalidate" (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")))))
      it "Parse PreOp" $ parse "++ invalidate a[123].test" `shouldBe` Right (Expr (PreUni "++" (PreUni "invalidate" (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")))))
