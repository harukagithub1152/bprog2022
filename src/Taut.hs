module Taut where

import Data.List ( nub )

data Prop
    = Const Bool
    | Var Char
    | Not Prop
    | Prop :/\: Prop 
    | Prop :=> Prop
    deriving (Eq, Show)

p1 :: Prop
p1 = Var 'A':/\: Not (Var 'A')

p2 :: Prop
p2 = (Var 'A' :/\: (Var 'A' :=> Var 'B')) :=> Var 'A'

p3 :: Prop
p3 = Var 'A' :=> (Var 'A' :/\: Var 'B')

p4 :: Prop
p4 = (Var 'A' :/\: (Var 'A' :=> Var 'B')) :=> Var 'B'

type Assoc a b = [(a, b)]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)  = b
eval s (Var x)    = find x s
eval s (Not p)    = not (eval s p)
eval s (p :/\: q) = eval s p && eval s q
eval s (p :=> q)  = eval s p <= eval s q

find :: Eq a => a -> Assoc a b -> b
find x [] = error "unbound Variable"
find x ((y,b):rs) 
    | x == y    = b
    | otherwise = find x rs

vars :: Prop -> [Char]
vars (Const _)  = []
vars (Var x)    = [x]
vars (Not p)    = vars p
vars (p :/\: q) = vars p ++ vars q
vars (p :=> q)  = vars p ++ vars q

{- 
bools :: Int -> [[Bool]]
bools n = map phi range
    where
        range = [0 .. 2^n - 1]
        phi   = map conv . take n . (++ repeat 0) . int2bin

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

conv :: Int -> Bool
conv 0 = False
conv 1 = True
-}

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
    where
        bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))  
    where
        vs = nub (vars p)

isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p ]