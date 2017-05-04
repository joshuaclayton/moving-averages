module Data.MovingAverage.SimpleSpec where

import Data.MovingAverage (simple)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "simple" $ do
        it "correctly calculates the average of an empty list" $
            simple 5 [] `shouldBe` []

        it "correctly calculates the average of a present list and small window" $
            simple 1 [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

        it "correctly calculates the average of a present list and medium window" $
            simple 2 [1, 2, 3, 4] `shouldBe` [1, 1.5, 2.5, 3.5]

        it "correctly calculates the average of a present list and large window" $
            simple 4 [1, 2, 3, 4] `shouldBe` [1, 1.5, 2, 2.5]

        it "correctly calculates the average of a present list and excessive window" $
            simple 100 [1, 2, 3, 4] `shouldBe` [1, 1.5, 2, 2.5]
