module Data.MovingAverage.Simple
    ( simple
    ) where

simple :: Fractional a => Int -> [a] -> [a]
simple _ [] = []
simple n samples
  | n <= 0 = []
  | otherwise =
      map fst3 $ scanl1 average sampleTriples
      where
        divisors = map fromIntegral $ [1..n] ++ repeat n
        nAgos = replicate (n - 1) 0 ++ samples
        sampleTriples = zip3 samples divisors nAgos

average :: Fractional a => (a, a, a) -> (a, a, a) -> (a, a, a)
average (prevAvg, prevDiv, dropMe) (sample, divisor, nAgo) =
    (newAvg, divisor, nAgo)
  where
    prevSum = prevAvg * prevDiv
    newAvg = (prevSum + sample - dropMe) / divisor

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
