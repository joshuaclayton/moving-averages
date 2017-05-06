module Data.MovingAverage.SingleExponential
    ( singleExponential
    ) where

import Data.MovingAverage.Types (ExponentialError(..), SmoothedResults, buildResults)

singleExponential :: (Ord a, Floating a) => a -> [a] -> Either ExponentialError (SmoothedResults a)
singleExponential _ [] = Left NoValuesProvided
singleExponential alpha xs
    | inRange 0 1 alpha = Right $ buildResults $ scanl go initialState (tail xs)
    | otherwise = Left $ InvalidAlphaValue "Alpha must be 0 <= a <= 1"
  where
    initialState = (head xs, head xs)
    go (_, previous) current = (current, s_t current previous)
    s_t current previous = alpha * current + (1 - alpha) * previous

inRange :: Ord a => a -> a -> a -> Bool
inRange min' max' value = value >= min' && value <= max'
