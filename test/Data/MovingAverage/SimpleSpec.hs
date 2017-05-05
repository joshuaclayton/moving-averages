module Data.MovingAverage.SimpleSpec where

import qualified Data.Maybe as M
import           Data.MovingAverage (SmoothedResult(..), SmoothedResults(..), simple)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "simple" $ do
        it "correctly calculates the average of an empty list" $
            simple 5 ([] :: [Float]) `shouldBe` Nothing

        it "correctly calculates the average of a present list and small window" $
            resultValues (simple 1 [1, 2, 3, 4]) `shouldBe` [1, 2, 3, 4]

        it "correctly calculates the average of a present list and medium window" $
            resultValues (simple 2 [1, 2, 3, 4]) `shouldBe` [1, 1.5, 2.5, 3.5]

        it "correctly calculates the average of a present list and large window" $
            resultValues (simple 4 [1, 2, 3, 4]) `shouldBe` [1, 1.5, 2, 2.5]

        it "correctly calculates the average of a present list and excessive window" $
            resultValues (simple 100 [1, 2, 3, 4]) `shouldBe` [1, 1.5, 2, 2.5]

resultValues :: Floating a => Maybe (SmoothedResults a) -> [a]
resultValues = map srSmoothedValue . srsResults . M.fromJust
