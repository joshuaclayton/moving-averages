module Test.TestHelper
    ( resultValues
    , graphName
    ) where

import Data.MovingAverage

resultValues :: Floating a => Either MovingAverageError (SmoothedResults a) -> [a]
resultValues = map srSmoothedValue . srsResults . right

graphName :: (Show a, Floating a) => Either MovingAverageError (SmoothedResults a) -> String
graphName = show . srsGraphType . right

right :: Either a b -> b
right (Right v) = v
