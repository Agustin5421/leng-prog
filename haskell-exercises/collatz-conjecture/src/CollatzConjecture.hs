module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz a = if a <= 0 then Nothing else Just (collatz' a 0)
  where
    collatz' a b
      | a == 1 = b
      | even a = collatz' (a `div` 2) (b + 1)
      | odd a = collatz' (3 * a + 1) (b + 1)
