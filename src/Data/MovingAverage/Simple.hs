module Data.MovingAverage.Simple
    ( simple
    ) where

import Data.MovingAverage.Types (SmoothedResults, MovingAverageError(..), buildSimpleMovingAverage)

simple :: Floating a => Int -> [a] -> Either MovingAverageError (SmoothedResults a)
simple _ [] = Left NoValuesProvided
simple n xs
  | n <= 0 = Left $ InvalidWindow "Window must be greater than 0"
  | otherwise =
        Right $ buildSimpleMovingAverage n xsAndSmoothedPairs
      where
        xsAndSmoothedPairs = zip xs (map fst3 $ scanl1 average sampleTriples)
        divisors = map fromIntegral $ [1..n] ++ repeat n
        nAgos = replicate (n - 1) 0 ++ xs
        sampleTriples = zip3 xs divisors nAgos

average :: Fractional a => (a, a, a) -> (a, a, a) -> (a, a, a)
average (prevAvg, prevDiv, dropMe) (sample, divisor, nAgo) =
    (newAvg, divisor, nAgo)
  where
    prevSum = prevAvg * prevDiv
    newAvg = (prevSum + sample - dropMe) / divisor

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
