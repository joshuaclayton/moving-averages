module Data.MovingAverage
    ( SmoothedResults(..)
    , SmoothedResult(..)
    , ExponentialError(..)

    , simple
    , singleExponential
    , doubleExponential
    ) where

import Data.MovingAverage.DoubleExponential (doubleExponential)
import Data.MovingAverage.Simple (simple)
import Data.MovingAverage.SingleExponential ( singleExponential)
import Data.MovingAverage.Types (ExponentialError(..), SmoothedResults(..), SmoothedResult(..))
