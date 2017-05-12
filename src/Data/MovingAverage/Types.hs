module Data.MovingAverage.Types
    ( SmoothedResults(..)
    , SmoothedResult(..)
    , MovingAverageError(..)
    , buildSimpleMovingAverage
    , buildSingleExponentialMovingAverage
    , buildDoubleExponentialMovingAverage
    ) where

newtype Window = Window Int deriving Eq
newtype Alpha a = Alpha a deriving Eq
newtype Beta b = Beta b deriving Eq

data GraphType a
    = SimpleMovingAverage Window
    | SingleExponentialMovingAverage (Alpha a)
    | DoubleExponentialMovingAverage (Alpha a) (Beta a)
    deriving Eq

instance Show a => Show (GraphType a) where
    show (SimpleMovingAverage (Window w)) = "SMA(" ++ show w ++ ")"
    show (SingleExponentialMovingAverage (Alpha a)) = "SEMA(" ++ show a ++ ")"
    show (DoubleExponentialMovingAverage (Alpha a) (Beta b)) = "DEMA(" ++ show a ++ ", " ++ show b ++ ")"

data SmoothedResults a = SmoothedResults
    { srsGraphType :: GraphType a
    , srsResults :: [SmoothedResult a]
    , srsSumSquaredErrors :: a
    , srsMeanSquaredErrors :: a
    } deriving (Eq, Show)

data SmoothedResult a = SmoothedResult
    { srValue :: a
    , srSmoothedValue :: a
    , srError :: a
    , srErrorSquared :: a
    } deriving (Eq, Show)

data MovingAverageError
    = InvalidAlphaValue String
    | InvalidBetaValue String
    | NoValuesProvided
    | InvalidWindow String
    deriving (Eq, Show)

buildSimpleMovingAverage :: Floating a => Int -> [(a, a)] -> SmoothedResults a
buildSimpleMovingAverage w = buildResults (SimpleMovingAverage (Window w))

buildSingleExponentialMovingAverage :: Floating a => a -> [(a, a)] -> SmoothedResults a
buildSingleExponentialMovingAverage a = buildResults (SingleExponentialMovingAverage (Alpha a))

buildDoubleExponentialMovingAverage :: Floating a => a -> a -> [(a, a)] -> SmoothedResults a
buildDoubleExponentialMovingAverage a b = buildResults (DoubleExponentialMovingAverage (Alpha a) (Beta b))

buildResults :: Floating a => GraphType a -> [(a, a)] -> SmoothedResults a
buildResults g = buildSmoothedResults g . map (uncurry buildSmoothedResult)

buildSmoothedResults :: Floating a => GraphType a -> [SmoothedResult a] -> SmoothedResults a
buildSmoothedResults g xs = SmoothedResults g xs sumSquaredErrors meanSquaredErrors
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
