module DisplaySpec (spec) where

import Test.Hspec

import Display
import Board

spec :: Spec
spec = do
  describe "squaresDisplayOrder" $ do
    it "возвращает 8 рядов" $
      length squaresDisplayOrder `shouldBe` 8

    it "первое поле должено быть 'a8'" $
      (head $ head $ squaresDisplayOrder) `shouldBe` Square 56

    it "последнее поле в первом ряду должено быть 'h8'" $
      (last $ head $ squaresDisplayOrder) `shouldBe` Square 63

    it "первое поле в последнем ряду должено быть 'a1'" $
      (head $ last $ squaresDisplayOrder) `shouldBe` Square 0

    it "последнее поле должено быть 'h1'" $
      (last $ last $ squaresDisplayOrder) `shouldBe` Square 7

