module Data.MovingAverage.Types
    ( SmoothedResults(..)
    , SmoothedResult(..)
    , buildSmoothedResults
    , buildSmoothedResult
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
