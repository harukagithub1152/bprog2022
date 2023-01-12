module AbstractMachine where

data Expr
    = val Int
    | Add Expr Expr
    deriving (Eq, Show)

interp :: Expr -> Int
interp e = case e of
    Val n   -> n
    Add x y -> interp x + interp y

sample :: Expr
sample = Add (Add (Val 2) (Val 3)) (Val 4)

type Cont = [Op]

data Op
    = EVAL Expr
    | ADD int
    deriving (Eq, Show)

eval :: Expr -> Cont -> Int
eval e c = case e of
    Val n -> exec c n
    Add x y -> eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec c n = case c of
    []            -> n
    EVAL y : c'   -> eval y (ADD n : c')
    ADD m : c'    -> exec c' (m + n)

value :: Expr -> Int
value e = eval e []