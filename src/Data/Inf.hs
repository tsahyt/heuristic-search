module Data.Inf where

data Inf a = NegInf | Fin !a | PosInf
    deriving (Show, Eq)

instance Ord a => Ord (Inf a) where
    PosInf   <= _        = False
    NegInf   <= _        = True
    _        <= PosInf   = True
    _        <= NegInf   = False
    Fin a    <= Fin b = a <= b

instance Num a => Num (Inf a) where
    Fin a + Fin b = Fin (a + b)
    PosInf + NegInf = error "undefined: PosInf + NegInf"
    NegInf + PosInf = error "undefined: NegInf + PosInf"
    PosInf + _ = PosInf
    _ + PosInf = PosInf
    NegInf + _ = NegInf
    _ + NegInf = NegInf

    Fin a * Fin b = Fin (a * b)
    PosInf * NegInf = NegInf
    NegInf * NegInf = PosInf
    PosInf * _ = PosInf
    NegInf * _ = NegInf
    _ * PosInf = PosInf
    _ * NegInf = NegInf

    negate (Fin a) = Fin (negate a)
    negate PosInf = NegInf
    negate NegInf = PosInf

    abs (Fin a) = Fin (abs a)
    abs _ = PosInf

    signum (Fin a) = Fin (signum a)
    signum NegInf = Fin (-1)
    signum PosInf = Fin 1

    fromInteger = Fin . fromInteger
