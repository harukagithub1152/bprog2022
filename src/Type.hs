module Type where

data Hosi 
    = Kuro
    | Siro
    deriving (Eq,Ord)

instance Show Hosi where
    show Kuro = "●"
    show Siro = "○"

instance Read Hosi where
    readsPrec _ ('●':cs) = [(Kuro, cs)]
    readsPrec _ ('○':cs) = [(Siro, cs)]
    readsPrec _ _        = []