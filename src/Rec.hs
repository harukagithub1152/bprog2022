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


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs 
merge (x:xs) (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = case halve xs of
    (ys, zs) -> merge (msort ys) (msort zs)

halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs)
    where
        h = length xs `div` 2

sample :: [Int]
sample = [3,1,4,1,5,9,2,6,5,3,5,8,9]

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

inc :: Int -> Int
inc x = x + 1
