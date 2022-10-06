module Rec where

import Prelude hiding (product, drop, init)

product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

init :: [a] -> [a]
init [] = error "karaya-"
init [_] = []
init (x:xs) = x : init xs 

fac :: Int -> Int
fac 0 = 1
fac n 
     | n > 0      = n * (n-1)
     | otherwise = error "negative number"