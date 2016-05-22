{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec where

import Test.Hspec
import qualified Language.TJS.Parser as P
import Language.TJS.Tree
import Text.ParserCombinators.Parsec.Error (ParseError)
import Data.Text

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

--instance Eq ParseError where
--    a == b = errorMessages a == errorMessages b

parse :: Text -> Either ParseError Stmt
parse = P.parse "<TEST>"

spec :: Spec
spec = do
  describe "num literal test" $ do
    it "Parse Dec Int" $ parse "123;" `shouldBe` Right (Exec (Int 123 NoSrcSpan) NoSrcSpan)
    it "Parse Dec Int With Comment" $ parse "123; //here is the comment" `shouldBe` Right (Exec (Int 123 NoSrcSpan) NoSrcSpan)
    it "Parse Dec Int" $ parse "123e2;" `shouldBe` Right (Exec (Int 12300 NoSrcSpan) NoSrcSpan)
    it "Parse Dec Int" $ parse "123.0e2;" `shouldBe` Right (Exec (Real 12300.0 NoSrcSpan) NoSrcSpan)
    it "Parse Dec Int" $ parse "0.01;" `shouldBe` Right (Exec (Real 0.01 NoSrcSpan) NoSrcSpan)
    it "Parse Oct Int" $ parse "0123;" `shouldBe` Right (Exec (Int 0o123 NoSrcSpan) NoSrcSpan)
    it "Parse Hex Int" $ do
      parse "0x123;" `shouldBe` Right (Exec (Int 0x123 NoSrcSpan) NoSrcSpan)
      parse "0X123;" `shouldBe` Right (Exec (Int 0x123 NoSrcSpan) NoSrcSpan)

  describe "str literal test" $ do
    it "Parse Single Str" $ parse "'anata to java';" `shouldBe` Right (Exec (Str "anata to java" NoSrcSpan) NoSrcSpan)
    it "Parse Single Str With Comment" $ parse "'anata to java' /* test */;" `shouldBe` Right (Exec (Str "anata to java" NoSrcSpan) NoSrcSpan)
    it "Parse Double Str" $ parse "\"anata to java\";" `shouldBe` Right (Exec (Str "anata to java" NoSrcSpan) NoSrcSpan)
    it "Parse Double Str" $ parse "\"anata \\\"to java\";" `shouldBe` Right (Exec (Str "anata \"to java" NoSrcSpan ) NoSrcSpan)
    it "Parse Double Str" $ parse "\"anata 'to java\";" `shouldBe` Right (Exec (Str "anata 'to java" NoSrcSpan) NoSrcSpan)
    it "Parse Double Str" $ parse "\"anata \\n\\nto java\";" `shouldBe` Right (Exec (Str "anata \n\nto java" NoSrcSpan) NoSrcSpan)
    it "Parse Double Str In Japanese" $ parse "\"あなたとジャバ\";" `shouldBe` Right (Exec (Str "あなたとジャバ" NoSrcSpan) NoSrcSpan)
    it "Parse Separated Double Str In Japanese" $ parse "\"あなたと\"  \"ジャバ\";" `shouldBe` Right (Exec (Str "あなたとジャバ" NoSrcSpan) NoSrcSpan)

  describe "identifer test" $ do
    it "Parse Identifer" $ parse "abc123;" `shouldBe` Right (Exec (Ident (Identifer "abc123") NoSrcSpan) NoSrcSpan)
    it "Parse Identifer" $ parse "__abc123__;" `shouldBe` Right (Exec (Ident (Identifer "__abc123__") NoSrcSpan) NoSrcSpan)
    it "Parse Identifer" $ parse "1__abc123__;" `shouldSatisfy` isLeft

  describe "postop" $ do
    it "Parse PostOp" $ parse "a[123];" `shouldBe` Right (Exec (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) NoSrcSpan)
    it "Parse PostOp" $ parse "あ[123].test;" `shouldBe` Right (Exec (Dot (Index (Ident (Identifer "あ") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) (Identifer "test") NoSrcSpan) NoSrcSpan)
    it "Parse PostOp" $ parse "a[123]++--!;" `shouldBe` Right (Exec (PostUni (PostUni (PostUni (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) "++" NoSrcSpan) "--" NoSrcSpan) "!" NoSrcSpan) NoSrcSpan)
    it "Parse PostOp" $ parse "a[123].test++--! ;" `shouldBe` Right (Exec (PostUni (PostUni (PostUni (Dot (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) (Identifer "test") NoSrcSpan) "++" NoSrcSpan) "--" NoSrcSpan) "!" NoSrcSpan) NoSrcSpan)

  describe "preop" $ do
    it "Parse PreOp" $ parse "++a[123];" `shouldBe` Right (Exec (PreUni "++" (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) NoSrcSpan) NoSrcSpan)
    it "Parse PreOp" $ parse "++ invalidate a[123].test;" `shouldBe` Right (Exec (PreUni "++" (PreUni "invalidate" (Dot (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) (Identifer "test") NoSrcSpan)NoSrcSpan)NoSrcSpan)NoSrcSpan)
    it "Parse PreOp" $ parse "++ invalidate a[123].test;" `shouldBe` Right (Exec (PreUni "++" (PreUni "invalidate" (Dot (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) (Identifer "test") NoSrcSpan)NoSrcSpan)NoSrcSpan)NoSrcSpan)

  describe "Func Apply" $ do
    it "Apply Normal" $ parse "a(123 );" `shouldBe` Right (Exec (Call (Ident (Identifer "a")NoSrcSpan) [ApplyExpr (Int 123 NoSrcSpan)] NoSrcSpan) NoSrcSpan)
    it "Apply All" $ parse "a(...);" `shouldBe` Right (Exec (Call (Ident (Identifer "a")NoSrcSpan) [ApplyLeft]NoSrcSpan)NoSrcSpan)
    it "Apply ArrayAll" $ parse "a(xs*);" `shouldBe` Right (Exec (Call (Ident (Identifer "a")NoSrcSpan) [ApplyArray (Ident (Identifer "xs")NoSrcSpan)]NoSrcSpan)NoSrcSpan)

  describe "complex expr" $ do
    it "Parse PreOp" $ parse "x ? 1 : 0;" `shouldBe` Right (Exec
            (Tri (Ident (Identifer "x")NoSrcSpan)
                  (Int 1 NoSrcSpan)
                  (Int 0 NoSrcSpan) NoSrcSpan)
                  NoSrcSpan)
    it "Parse PreOp" $ parse "x ? a ? b : c : 0;" `shouldBe` Right (Exec
            (Tri (Ident (Identifer "x") NoSrcSpan)
                  (Tri (Ident (Identifer "a") NoSrcSpan) (Ident (Identifer "b") NoSrcSpan) (Ident (Identifer "c") NoSrcSpan) NoSrcSpan)
                  (Int 0 NoSrcSpan) NoSrcSpan)
                  NoSrcSpan)
    it "Parse PreOp" $ parse "x instanceof ++a[123] ? y ? z : 1 : 0;" `shouldBe` Right (Exec
            (Bin "instanceof" (Ident (Identifer "x") NoSrcSpan)
              (Tri (PreUni "++" (Index (Ident (Identifer "a") NoSrcSpan) (Int 123 NoSrcSpan) NoSrcSpan) NoSrcSpan)
                  (Tri (Ident (Identifer "y") NoSrcSpan) (Ident (Identifer "z") NoSrcSpan) (Int 1 NoSrcSpan) NoSrcSpan)
                  (Int 0 NoSrcSpan) NoSrcSpan) NoSrcSpan) NoSrcSpan)
    it "Parse BinOp" $ do
      parse "x *(y);" `shouldBe` Right (Exec (Bin "*" (Ident (Identifer "x") NoSrcSpan) (Ident (Identifer "y") NoSrcSpan) NoSrcSpan) NoSrcSpan)
      parse "x != y;" `shouldBe` Right (Exec (Bin "!=" (Ident (Identifer "x") NoSrcSpan) (Ident (Identifer "y") NoSrcSpan) NoSrcSpan) NoSrcSpan)
  describe "Parse Statements" $ do
    it "Parse If" $ do
      parse "if (1) x;" `shouldBe` Right (If (Int 1 NoSrcSpan) (Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan) Nothing NoSrcSpan)
      parse "if (1) x; else y;" `shouldBe` Right (If (Int 1 NoSrcSpan) (Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan) (Just (Exec (Ident (Identifer "y") NoSrcSpan) NoSrcSpan)) NoSrcSpan)
    it "Parse While" $
      parse "while (1) x;" `shouldBe` Right (While (Int 1 NoSrcSpan) (Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan) NoSrcSpan)
    it "Parse With" $
      parse "with (obj) .x = 1;" `shouldBe` Right (With (Ident (Identifer "obj") NoSrcSpan) (Exec (Bin "=" (Dot (WithThis NoSrcSpan) (Identifer "x") NoSrcSpan) (Int 1 NoSrcSpan) NoSrcSpan) NoSrcSpan) NoSrcSpan)
    it "Parse Switch" $ do
      parse "switch (1) {case 1: x; case 2: y;}" `shouldBe` Right (Switch (Int 1 NoSrcSpan) [(Int 1 NoSrcSpan, [Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan]), (Int 2 NoSrcSpan, [Exec (Ident (Identifer "y") NoSrcSpan) NoSrcSpan])] Nothing NoSrcSpan)
      parse "switch (1) {case 1: x; case 2: y; default: z; k;}" `shouldBe` Right (Switch (Int 1 NoSrcSpan) [(Int 1 NoSrcSpan, [Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan]), (Int 2 NoSrcSpan, [Exec (Ident (Identifer "y") NoSrcSpan) NoSrcSpan])] (Just [Exec (Ident (Identifer "z") NoSrcSpan) NoSrcSpan,Exec (Ident (Identifer "k") NoSrcSpan) NoSrcSpan]) NoSrcSpan)
    it "Parse Try" $ do
      parse "try x; catch (x) z;" `shouldBe` Right (Try (Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan) (Identifer "x") (Exec (Ident (Identifer "z") NoSrcSpan) NoSrcSpan) NoSrcSpan)
      parse "throw z;" `shouldBe` Right (Throw (Ident (Identifer "z") NoSrcSpan) NoSrcSpan)
    it "Parse Return" $
      parse "return z;" `shouldBe` Right (Return (Ident (Identifer "z") NoSrcSpan) NoSrcSpan)

    it "Parse For" $
      parse "for(x;y;z) {w; continue;}" `shouldBe` Right (For (Just (Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan)) (Just (Ident (Identifer "y") NoSrcSpan)) (Just (Ident (Identifer "z") NoSrcSpan)) (Block [Exec (Ident (Identifer "w") NoSrcSpan) NoSrcSpan, Continue NoSrcSpan] NoSrcSpan) NoSrcSpan)

    it "Parse Block" $
      parse "{ x ; y ; z;}" `shouldBe` Right (Block [Exec (Ident (Identifer "x") NoSrcSpan) NoSrcSpan,Exec (Ident (Identifer "y") NoSrcSpan) NoSrcSpan,Exec (Ident (Identifer "z") NoSrcSpan) NoSrcSpan] NoSrcSpan)

    it "Parse Var" $
      parse "var x=1,y,z=w;" `shouldBe` Right (Var [(Identifer "x", Just (Int 1 NoSrcSpan)), (Identifer "y", Nothing), (Identifer "z", Just (Ident (Identifer "w") NoSrcSpan))] NoSrcSpan)

    it "Parse Function" $ do
      parse "function f(a,b=1,a*) { return x; }" `shouldBe` Right (Func (Identifer "f") [FuncArg (Identifer "a") Nothing, FuncArg (Identifer "b") (Just (Int 1 NoSrcSpan)), FuncArray (Identifer "a")] (Block [Return (Ident (Identifer "x") NoSrcSpan) NoSrcSpan] NoSrcSpan) NoSrcSpan)
      parse "function (a,b=1,a*) { return x; };" `shouldBe` Right (Exec
              (AnonFunc
                [FuncArg (Identifer "a") Nothing, FuncArg (Identifer "b") (Just (Int 1 NoSrcSpan)), FuncArray (Identifer "a")]
                (Block [Return (Ident (Identifer "x") NoSrcSpan) NoSrcSpan] NoSrcSpan)
                NoSrcSpan)
            NoSrcSpan)

    it "Parse Prop" $ do
      parse "property x { getter { return z; } setter (y) { z = y; } }" `shouldBe` Right (
        Prop
          (Identifer "x")
          (Just (Block [Return (Ident (Identifer "z") NoSrcSpan) NoSrcSpan] NoSrcSpan))
          (Just (Identifer "y",Block [Exec (Bin "=" (Ident (Identifer "z") NoSrcSpan) (Ident (Identifer "y") NoSrcSpan) NoSrcSpan) NoSrcSpan] NoSrcSpan))
        NoSrcSpan)
      parse "property x { getter { return z; } }" `shouldBe` Right (
        Prop
          (Identifer "x")
          (Just (Block [Return (Ident (Identifer "z") NoSrcSpan) NoSrcSpan] NoSrcSpan))
          Nothing
          NoSrcSpan)
      parse "property x { setter (y) { z = y; } }" `shouldBe` Right (
        Prop
          (Identifer "x")
          Nothing
          (Just (Identifer "y",Block [Exec (Bin "=" (Ident (Identifer "z") NoSrcSpan) (Ident (Identifer "y") NoSrcSpan) NoSrcSpan) NoSrcSpan] NoSrcSpan))
          NoSrcSpan)


    it "Parse Class" $ do
      parse "class A{ var x; function Z() {} }" `shouldBe` Right (
          Class (Identifer "A")
          Nothing
          [Var [(Identifer "x", Nothing)] NoSrcSpan,Func (Identifer "Z") [] (Block [] NoSrcSpan) NoSrcSpan]
          NoSrcSpan)
      parse "class A extends X,Y,Z{ var x; function Z() {} }" `shouldBe` Right (
          Class (Identifer "A")
          (Just [Identifer "X",Identifer "Y",Identifer "Z"])
          [Var [(Identifer "x", Nothing)] NoSrcSpan,Func (Identifer "Z") [] (Block [] NoSrcSpan) NoSrcSpan]
          NoSrcSpan)
