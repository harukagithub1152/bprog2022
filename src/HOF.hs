module HOF where

import Data.Char

type Bit = Int

-- bin2int :: [Bit] -> Int
-- bin2int bits = sum [ w * b | (w, b) <- zip weights bits]
--     where
--         weights = iterate (2*) 1

-- iterate :: (a -> a) -> a -> [a]
-- iterate f x = x : iterate f (f x)

-- 1 3 4 8
-- [a,b,c,d]
-- a + 2*b + 2*2*c + 2*2*2*d + 2*2*2*2*0
-- a + 2*(b + 2*(c + 2*(d + 2*0)))

{- | bin2int
>>> bin2int [1,0,1,1,0,0,0,0] 
13
-}
bin2int :: [Bit] -> Int
bin2int = foldr f 0
    where
        f x y = x + 2*y

{- | int2bin
>>> int2bin 13
[1,0,1,1]
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{- | make8
>>> make8 (int2bin 13)
[1,0,1,1,0,0,0,0]
-}
make8 :: [Bit] -> [Bit]
make8 xs = take 8 (xs ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode =  map (chr . bin2int) . chop8

chop8 :: [Bit] -> [[Bit]]
chop8[] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)
