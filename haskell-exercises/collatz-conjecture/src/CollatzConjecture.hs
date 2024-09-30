module CollatzConjecture (collatz) where

collatz :: Integer -> Integer
collatz a = collatz1 a 0
collatz1 a b
        | a == 1 = b
        | even a = collatz1 (a `div` 2) (b+1)
        | odd a = collatz1 (3 * a + 1) (b + 1)