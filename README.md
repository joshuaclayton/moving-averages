# moving-averages

This is a library for calculating moving averages on lists of numbers.

## Usage

```haskell
import qualified Data.MovingAverage as MA

resultValues :: Floating a => MA.SmoothedResults a -> [a]
resultValues = map MA.srSmoothedValue . MA.srsResults

-- window of 2
resultValues <$> MA.simple 2 [5, 10, 20, 2]
-- Just [5.0, 7.5, 15.0, 11.0]

-- alpha of 0.5
resultValues <$> MA.singleExponential 0.5 [1, 2, 3, 4, 5]
-- Right [1, 1.5, 2.25, 3.125, 4.0625]

-- alpha of 0.5, beta of 0.1
resultValues <$> MA.doubleExponential 0.5 0.1 [3, 1, 4, 6, 8]
-- Right [3.0, 1.0, 1.5, 2.875, 4.71875]
```

## License

Copyright 2017 Josh Clayton. See the [LICENSE](LICENSE).
