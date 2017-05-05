module Data.MovingAverage.SingleExponentialSpec where

import qualified Data.Maybe as M
import           Data.MovingAverage (SmoothedResults, SingleExponentialError(..), srsResults, srSmoothedValue, singleExponential)
import           Test.Hspec

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
            let (Right result) = singleExponential 0.5 [1..5]
            resultValues result `shouldBe` [1, 1, 1.5, 2.25, 3.125]

resultValues :: Floating a => SmoothedResults a -> [a]
resultValues = map srSmoothedValue . srsResults
