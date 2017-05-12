module Data.MovingAverage.DoubleExponentialSpec where

import Data.MovingAverage (SmoothedResults, MovingAverageError(..), srsResults, srSmoothedValue, doubleExponential)
import Test.Hspec
import Test.TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "doubleExponential" $ do
        it "correctly handles when no values are provided" $ do
            let (Left result) = doubleExponential 0.1 0.1 ([] :: [Float])
            result `shouldBe` NoValuesProvided

        it "correctly handles when the alpha value is incorrect" $ do
            let (Left result) = doubleExponential 2 0.1 [1..5]
            result `shouldBe` InvalidAlphaValue "Alpha must be 0 < a < 1"

        it "correctly handles when the beta value is incorrect" $ do
            let (Left result) = doubleExponential 0.1 2 [1..5]
            result `shouldBe` InvalidBetaValue "Beta must be 0 < b < 1"

        it "correctly calculates values" $ do
            let result = doubleExponential 0.5 0.5 [3,1,3,1,3,1]
            resultValues result `shouldBe` [3.0, 1.0, 1.0, 0.5, 1.375, 1.21875]

        it "correctly shows the name of the average" $
            graphName (doubleExponential 0.5 0.2 [1, 2, 3]) `shouldBe` "DEMA(0.5, 0.2)"
