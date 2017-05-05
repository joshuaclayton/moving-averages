module Data.MovingAverage
    ( SmoothedResults(..)
    , SmoothedResult(..)
    , SingleExponentialError(..)

    , simple
    , singleExponential
    ) where

import Data.MovingAverage.Simple (simple)
import Data.MovingAverage.SingleExponential (SingleExponentialError(..), singleExponential)
import Data.MovingAverage.Types (SmoothedResults(..), SmoothedResult(..))
