module Data.MovingAverage.SimpleSpec where

import Data.MovingAverage (SmoothedResult(..), SmoothedResults(..), MovingAverageError(..), simple)
import Test.Hspec
import Test.TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "simple" $ do
        it "correctly calculates the average of an empty list" $
            simple 5 ([] :: [Float]) `shouldBe` Left NoValuesProvided

        it "correctly calculates the average of an empty list" $
            simple 0 [1,2,3,4] `shouldBe` (Left $ InvalidWindow "Window must be greater than 0")

        it "correctly calculates the average of a present list and small window" $
            resultValues (simple 1 [1, 2, 3, 4]) `shouldBe` [1, 2, 3, 4]

        it "correctly calculates the average of a present list and medium window" $
            resultValues (simple 2 [1, 2, 3, 4]) `shouldBe` [1, 1.5, 2.5, 3.5]

        it "correctly calculates the average of a present list and large window" $
            resultValues (simple 4 [1, 2, 3, 4]) `shouldBe` [1, 1.5, 2, 2.5]

        it "correctly calculates the average of a present list and excessive window" $
            resultValues (simple 100 [1, 2, 3, 4]) `shouldBe` [1, 1.5, 2, 2.5]

        it "correctly shows the name of the average" $
            graphName (simple 100 [1, 2, 3, 4]) `shouldBe` "SMA(100)"
