module NonZero where

data NZ = One | S NZ deriving (Eq, Ord, Show)

mult :: NZ -> NZ -> NZ
mult One y = y
mult (S x) y = add y (mult x y)

add :: NZ -> NZ -> NZ
add One y = S y
add (S x) y = add x (S y)

instance Enum NZ where
    fromEnum One = 1
    fromEnum (S x) = 1 + fromEnum x
    toEnum 0 = undefined
    toEnum 1 = One
    toEnum n = S $ toEnum (n-1)
