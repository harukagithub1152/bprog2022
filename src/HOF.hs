module HOF where

import Data.Char

type Bit = Int

{- |
>>> bin2int [1,0,1,1]
13
-}
bin2int :: [Bit] -> Int
bin2int bits = sum [ w * b | (w, b) <- zip weights bits]
    where
        weights = iterate (2*) 1

-- iterate :: (a -> a) -> a -> [a]
-- iterate f x = x : iterate f (f x)
