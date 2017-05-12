module Data.MovingAverage.SingleExponentialSpec where

import Data.MovingAverage (SmoothedResults, MovingAverageError(..), srsResults, srSmoothedValue, singleExponential)
import Test.Hspec
import Test.TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "singleExponential" $ do
        it "correctly handles when no values are provided" $ do
            let (Left result) = singleExponential 0.1 ([] :: [Float])
            result `shouldBe` NoValuesProvided

        it "correctly handles when the alpha value is incorrect" $ do
            let (Left result) = singleExponential 2 [1..5]
            result `shouldBe` InvalidAlphaValue "Alpha must be 0 <= a <= 1"

        it "correctly handles when the alpha value is incorrect" $ do
            let result = singleExponential 0.5 [1..5]
            resultValues result `shouldBe` [1, 1.5, 2.25, 3.125, 4.0625]

        it "correctly shows the name of the average" $
            graphName (singleExponential 0.2 [1, 2, 3]) `shouldBe` "SEMA(0.2)"
