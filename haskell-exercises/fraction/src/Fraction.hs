module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

-- Implement the `add` Function
add :: Fraction -> Fraction -> Fraction
add (a,b) (c,d) = simplify (e, f)
  where
    e = (a * d) + (c * b)
    f = (b * d)

-- Implement the `sub` Function
sub :: Fraction -> Fraction -> Fraction
sub (a,b) (c,d) = simplify (e,f)
  where
    e = (a * d) - (c * b)
    f = (b * d)

-- Implement the `mul` Function
mul :: Fraction -> Fraction -> Fraction
mul (a,b) (c,d) = simplify (e,f)
  where
    e = (a * c)
    f = (b * d)

-- Implement the `divide` Function
divide :: Fraction -> Fraction -> Fraction
divide (a,b) (c,d) = simplify (e,f)
  where
    e = (a * d)
    f = (b * c)

-- Implement the Highest Common Factor Function
hcf :: Int -> Int -> Int
hcf a b = if b == 0 then a else hcf b (a `mod` b)

simplify :: Fraction -> Fraction
simplify (n, d) = (n `div` h, d `div` h)
  where h = hcf n d