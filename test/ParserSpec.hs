{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec where

import Test.Hspec
import qualified Parser.Parser as P
import Parser.Tree
import Text.ParserCombinators.Parsec.Error (ParseError)

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

--instance Eq ParseError where
--    a == b = errorMessages a == errorMessages b

parse :: String -> Either ParseError Stmt
parse = P.parse "<TEST>"

spec :: Spec
spec = do
  describe "num literal test" $ do
    it "Parse Dec Int" $ parse "123;" `shouldBe` Right (Exec $ Int 123)
    it "Parse Dec Int With Comment" $ parse "123; //here is the comment" `shouldBe` Right (Exec $ Int 123)
    it "Parse Dec Int" $ parse "123e2;" `shouldBe` Right (Exec $ Int 12300)
    it "Parse Dec Int" $ parse "123.0e2;" `shouldBe` Right (Exec $ Real 12300.0)
    it "Parse Oct Int" $ parse "0123;" `shouldBe` Right (Exec $ Int 0o123)
    it "Parse Hex Int" $ do
      parse "0x123;" `shouldBe` Right (Exec $ Int 0x123)
      parse "0X123;" `shouldBe` Right (Exec $ Int 0x123)

  describe "str literal test" $ do
    it "Parse Single Str" $ parse "'anata to java';" `shouldBe` Right (Exec $ Str "anata to java")
    it "Parse Single Str With Comment" $ parse "'anata to java' /* test */;" `shouldBe` Right (Exec $ Str "anata to java")
    it "Parse Double Str" $ parse "\"anata to java\";" `shouldBe` Right (Exec $ Str "anata to java")
    it "Parse Double Str" $ parse "\"anata \\\"to java\";" `shouldBe` Right (Exec $ Str "anata \"to java")
    it "Parse Double Str" $ parse "\"anata 'to java\";" `shouldBe` Right (Exec $ Str "anata 'to java")
    it "Parse Double Str" $ parse "\"anata \\n\\nto java\";" `shouldBe` Right (Exec $ Str "anata \n\nto java")

  describe "identifer test" $ do
    it "Parse Identifer" $ parse "abc123;" `shouldBe` Right (Exec $ Ident $ Identifer "abc123")
    it "Parse Identifer" $ parse "__abc123__;" `shouldBe` Right (Exec $ Ident $ Identifer "__abc123__")
    it "Parse Identifer" $ parse "1__abc123__;" `shouldSatisfy` isLeft

  describe "postop" $ do
    it "Parse PostOp" $ parse "a[123];" `shouldBe` Right (Exec (Index (Ident (Identifer "a")) (Int 123)))
    it "Parse PostOp" $ parse "a[123].test;" `shouldBe` Right (Exec (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")))
    it "Parse PostOp" $ parse "a[123]++--!;" `shouldBe` Right (Exec (PostUni (PostUni (PostUni (Index (Ident (Identifer "a")) (Int 123)) "++") "--") "!"))
    it "Parse PostOp" $ parse "a[123].test++--! ;" `shouldBe` Right (Exec (PostUni (PostUni (PostUni (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")) "++") "--") "!"))

  describe "preop" $ do
    it "Parse PreOp" $ parse "++a[123];" `shouldBe` Right (Exec (PreUni "++" (Index (Ident (Identifer "a")) (Int 123))))
    it "Parse PreOp" $ parse "++ invalidate a[123].test;" `shouldBe` Right (Exec (PreUni "++" (PreUni "invalidate" (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")))))
    it "Parse PreOp" $ parse "++ invalidate a[123].test;" `shouldBe` Right (Exec (PreUni "++" (PreUni "invalidate" (Dot (Index (Ident (Identifer "a")) (Int 123)) (Identifer "test")))))

  describe "complex expr" $ do
    it "Parse PreOp" $ parse "x ? 1 : 0;" `shouldBe` Right (Exec
            (Tri (Ident $ Identifer "x")
                  (Int 1)
                  (Int 0)))
    it "Parse PreOp" $ parse "x ? a ? b : c : 0;" `shouldBe` Right (Exec
            (Tri (Ident $ Identifer "x")
                  (Tri (Ident $ Identifer "a") (Ident $ Identifer "b") (Ident $ Identifer "c"))
                  (Int 0)))
    it "Parse PreOp" $ parse "x instanceof ++a[123] ? y ? z : 1 : 0;" `shouldBe` Right (Exec
            (Bin "instanceof" (Ident $ Identifer "x")
              (Tri (PreUni "++" (Index (Ident (Identifer "a")) (Int 123)))
                  (Tri (Ident $ Identifer "y") (Ident $ Identifer "z") (Int 1))
                  (Int 0))))
  describe "complex expr" $ do
    it "Parse If" $ do
      parse "if (1) x;" `shouldBe` Right (If (Int 1) (Exec (Ident (Identifer "x"))) Nothing)
      parse "if (1) x; else y;" `shouldBe` Right (If (Int 1) (Exec (Ident (Identifer "x"))) (Just (Exec (Ident (Identifer "y")))))
