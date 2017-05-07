module Data.MovingAverage
    ( SmoothedResults(..)
    , SmoothedResult(..)
    , MovingAverageError(..)

    , simple
    , singleExponential
    , doubleExponential
    ) where

import Data.MovingAverage.DoubleExponential (doubleExponential)
import Data.MovingAverage.Simple (simple)
import Data.MovingAverage.SingleExponential ( singleExponential)
import Data.MovingAverage.Types (MovingAverageError(..), SmoothedResults(..), SmoothedResult(..))
