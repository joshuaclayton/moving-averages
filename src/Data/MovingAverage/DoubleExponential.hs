module Data.MovingAverage.DoubleExponential
    ( doubleExponential
    ) where

import Data.MovingAverage.Types (SmoothedResults, ExponentialError(..), buildResults)

doubleExponential :: (Ord a, Floating a) => a -> a -> [a] -> Either ExponentialError (SmoothedResults a)
doubleExponential _ _ [] = Left NoValuesProvided
doubleExponential alpha beta xs =
    case (inRange 0 1 alpha, inRange 0 1 beta) of
        (True, True) -> Right $ processResults $ scanl go initialState (tail xs)
        (False, _) -> Left $ InvalidAlphaValue "Alpha must be 0 < a < 1"
        (_, False) -> Left $ InvalidBetaValue "Beta must be 0 < b < 1"
  where
    processResults = buildResults . map pairFromTriple
    go (_, sPrevious, betaPrevious) current =
        ( current
        , s_t current sPrevious betaPrevious
        , b_t (s_t current sPrevious betaPrevious) sPrevious betaPrevious
        )
    initialState = (head xs, head xs, xs !! 1 - head xs)
    s_t y sPrevious betaPrevious = alpha * y + (1 - alpha) * (sPrevious + betaPrevious)
    b_t sCurrent sPrevious betaPrevious = beta * (sCurrent - sPrevious) + (1 - beta) * betaPrevious

inRange :: Ord a => a -> a -> a -> Bool
inRange min' max' value = value > min' && value < max'

pairFromTriple :: (a, b, c) -> (a, b)
pairFromTriple (a, b, _) = (a, b)
