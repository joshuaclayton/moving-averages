module Data.MovingAverage.Types
    ( SmoothedResults(..)
    , SmoothedResult(..)
    , ExponentialError(..)
    , buildResults
    ) where

data SmoothedResults a = SmoothedResults
    { srsResults :: [SmoothedResult a]
    , srsSumSquaredErrors :: a
    , srsMeanSquaredErrors :: a
    } deriving (Eq, Show)

data SmoothedResult a = SmoothedResult
    { srValue :: a
    , srSmoothedValue :: a
    , srError :: a
    , srErrorSquared :: a
    } deriving (Eq, Show)

data ExponentialError
    = InvalidAlphaValue String
    | InvalidBetaValue String
    | NoValuesProvided
    deriving (Eq, Show)

buildResults :: Floating a => [(a, a)] -> SmoothedResults a
buildResults = buildSmoothedResults . map (uncurry buildSmoothedResult)

buildSmoothedResults :: Floating a => [SmoothedResult a] -> SmoothedResults a
buildSmoothedResults xs = SmoothedResults xs sumSquaredErrors meanSquaredErrors
  where
    sumSquaredErrors = sum squaredErrors
    meanSquaredErrors = sumSquaredErrors / fromIntegral (length squaredErrors)
    squaredErrors = map srErrorSquared xs

buildSmoothedResult :: Floating a => a -> a -> SmoothedResult a
buildSmoothedResult v smoothed =
    SmoothedResult v smoothed smoothedError smoothedErrorSquared
  where
    smoothedError = v - smoothed
    smoothedErrorSquared = smoothedError ** 2
