module Positive where

data Positive = One | S Positive deriving (Eq, Ord, Show)

instance Num Positive where
    One + y = S y
    S x + y = x + S y
    One * y = y
    S x * y = y + x * y
    negate = error "Can't negate a positive number."
    abs = id
    signum _ = One
    fromInteger = toEnum . fromIntegral

instance Enum Positive where
    fromEnum One = 1
    fromEnum (S x) = 1 + fromEnum x
    toEnum 1 = One
    toEnum n | n > 0 = S $ toEnum (n-1)
             | otherwise = error "toEnum index must be positive."
