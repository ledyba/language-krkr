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

parse2 :: String -> Either ParseError Stmt
parse2 = P.parse2 "<TEST>"

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

  describe "Func Apply" $ do
    it "Apply Normal" $ parse "a(123 );" `shouldBe` Right (Exec (Call (Ident (Identifer "a")) [ApplyExpr (Int 123)]))
    it "Apply All" $ parse "a(...);" `shouldBe` Right (Exec (Call (Ident (Identifer "a")) [ApplyLeft]))
    it "Apply ArrayAll" $ parse "a(xs*);" `shouldBe` Right (Exec (Call (Ident (Identifer "a")) [ApplyArray (Ident (Identifer "xs"))]))

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
  describe "Parse Statements" $ do
    it "Parse If" $ do
      parse "if (1) x;" `shouldBe` Right (If (Int 1) (Exec (Ident (Identifer "x"))) Nothing)
      parse "if (1) x; else y;" `shouldBe` Right (If (Int 1) (Exec (Ident (Identifer "x"))) (Just (Exec (Ident (Identifer "y")))))
    it "Parse While" $
      parse "while (1) x;" `shouldBe` Right (While (Int 1) (Exec (Ident (Identifer "x"))))
    it "Parse With" $
      parse "with (obj) .x = 1;" `shouldBe` Right (With (Ident (Identifer "obj")) (Exec (Bin "=" (Dot WithThis (Identifer "x")) (Int 1))))
    it "Parse Switch" $ do
      parse "switch (1) {case 1: x; case 2: y;}" `shouldBe` Right (Switch (Int 1) [(Int 1, [Exec (Ident (Identifer "x"))]), (Int 2, [Exec (Ident (Identifer "y"))])] Nothing)
      parse "switch (1) {case 1: x; case 2: y; default: z; k;}" `shouldBe` Right (Switch (Int 1) [(Int 1, [Exec (Ident (Identifer "x"))]), (Int 2, [Exec (Ident (Identifer "y"))])] (Just [Exec (Ident (Identifer "z")),Exec (Ident (Identifer "k"))]))
    it "Parse Try" $ do
      parse "try x; catch (x) z;" `shouldBe` Right (Try (Exec (Ident (Identifer "x"))) (Identifer "x") (Exec (Ident (Identifer "z"))))
      parse "throw z;" `shouldBe` Right (Throw (Ident (Identifer "z")))
    it "Parse Return" $
      parse "return z;" `shouldBe` Right (Return (Ident (Identifer "z")))

    it "Parse For" $
      parse "for(x;y;z) {w; continue;}" `shouldBe` Right (For (Ident (Identifer "x")) (Ident (Identifer "y")) (Ident (Identifer "z")) (Block [Exec (Ident (Identifer "w")), Continue]))

    it "Parse Block" $
      parse "{ x ; y ; z;}" `shouldBe` Right (Block [Exec (Ident (Identifer "x")),Exec (Ident (Identifer "y")),Exec (Ident (Identifer "z"))])

    it "Parse Var" $
      parse "var x=1,y,z=w;" `shouldBe` Right (Var [(Identifer "x", Just (Int 1)), (Identifer "y", Nothing), (Identifer "z", Just (Ident (Identifer "w")))])

    it "Parse Function" $
      parse "function f(a,b=1,a*) { return x; }" `shouldBe` Right (Func (Just (Identifer "f")) [FuncArg (Identifer "a") Nothing, FuncArg (Identifer "b") (Just (Int 1)), FuncArray (Identifer "a")] (Block [Return (Ident (Identifer "x"))]))

    it "Parse Prop" $ do
      parse "property x { getter { return z; } setter (y) { z = y; } }" `shouldBe` Right (
        Prop
          (Identifer "x")
          (Just (Block [Return (Ident (Identifer "z"))]))
          (Just (Identifer "y",Block [Exec (Bin "=" (Ident (Identifer "z")) (Ident (Identifer "y")))]))
        )
      parse "property x { getter { return z; } }" `shouldBe` Right (
        Prop
          (Identifer "x")
          (Just (Block [Return (Ident (Identifer "z"))]))
          Nothing
        )
      parse "property x { setter (y) { z = y; } }" `shouldBe` Right (
        Prop
          (Identifer "x")
          Nothing
          (Just (Identifer "y",Block [Exec (Bin "=" (Ident (Identifer "z")) (Ident (Identifer "y")))]))
        )


    it "Parse Class" $
      parse "class A{ var x; function Z() {} }" `shouldBe` Right (
        Class (Identifer "A")
        [Var [(Identifer "x", Nothing)],Func (Just (Identifer "Z")) [] (Block [])]
      )
