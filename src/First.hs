module First where

double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple x = double (double x)