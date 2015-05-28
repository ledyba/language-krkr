module ParserSpec where

import Test.Hspec
import Parser.Parser (parse)

spec :: Spec
spec = do
  describe "hello test" $ do
    it "test" $ 3 `shouldBe` (3 :: Int)

  describe "hello test2" $ do
    it "hogehoge" $ "foo" `shouldBe` "foo"
