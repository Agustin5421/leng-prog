module Lists (member, union, intersection, difference,
              insert, insertionSort, firsts,
              binaryToDecimal, toDecimal, toDec, decimal,
              binaryAdd) where
  
import Data.Char (digitToInt, intToDigit)

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys


--1. List as Sets
-- Define the function `intersection` which takes two lists of integers and returns their common elements.
intersection :: [Int] -> [Int] -> [Int]
-- `xs` and `ys` are the two lists of integers.
intersection xs ys =
    -- Use a list comprehension to find common elements.
    [x | x <- xs, x `elem` ys]
    -- `x <- xs` iterates over each element `x` in the first list `xs`.
    -- `x \`elem\` ys` checks if `x` is in the second list `ys`.
    -- Only elements that satisfy this condition are included in the resulting list.
    -- []: This is the list constructor in Haskell. It is used to define a list.


difference:: [Int] -> [Int] -> [Int]
difference xs ys = [x | x <- xs, x 'notElem' ys]
    -- Similar to elem, notElem checks if x is not in ys

-- Define the function `insert` which takes an element and a sorted list, and inserts the element in the correct position.
insert:: Int -> [Int] -> [Int]
insert x [] = [x]  -- If the list is empty, the element `x` is the only element in the list.
insert x (y:ys)    -- (y:ys): Pattern matching on a non-empty list where y is the head and ys is the tail.
    | x <= y    = x : y : ys        -- If `x` is less than or equal to `y`, Constructs a new list with x before y, followed by the rest of the list
    | otherwise = y : insert x ys   -- Constructs a new list with 'y' as the head, followed by recursively inserting x into ys.
    --otherwise: A catch-all condition when previous guards fail.


--2. Insertion Sort
-- Define the function `insertionSort` which sorts a list using the `insert` function.
insertionSort :: [Int] -> [Int]
insertionSort [] = []  -- An empty list is already sorted.
insertionSort (x:xs) = insert x (insertionSort xs)
    -- Insert `x` into the sorted list obtained by recursively sorting the rest of the list `xs`.

{-
How does foldr work? How does it know which list it should iterate?
-- Define the function `insertionSort` using `foldr` to accumulate a sorted list.
insertionSort :: [Int] -> [Int]
insertionSort = foldr insert []
    -- `foldr` takes each element from the list and applies `insert` to build the sorted list.
-}

--Using zip
binaryToDecimal :: [Int] -> Int
binaryToDecimal bs = sum [x * 2^i | (x, i) <- zip (reverse bs) [0..]]
-- `reverse bs` reverses the list of binary digits so we can process from the least significant to the most significant.
-- `zip (reverse bs) [0..]` pairs each digit with its position index.
-- `[x * 2^i | (x, i) <- zip (reverse bs) [0..]]` calculates the decimal value by summing each digit multiplied by 2 raised to its position index.
-- `sum` adds up all these values to get the final decimal number.


--3. Numeral Systems
-- Converts a binary list to decimal
binaryToDecimal :: [Int] -> Int
binaryToDecimal bs = binaryToDecimalAux (reverse bs) 0
  where
    binaryToDecimalAux :: [Int] -> Int -> Int
    binaryToDecimalAux [] _ = 0
    binaryToDecimalAux (b:bs) power = b * 2^power + binaryToDecimalAux bs (power + 1)

-- Converts a number in a given base to decimal
toDecimal :: Int -> [Int] -> Int
toDecimal base digits = toDecimalAux (reverse digits) 0
  where
    toDecimalAux :: [Int] -> Int -> Int
    toDecimalAux [] _ = 0
    toDecimalAux (d:ds) power = d * base^power + toDecimalAux ds (power + 1)

-- Converts a string in a given base to decimal using `digitToInt`
toDec :: Int -> String -> Int
toDec base str = toDecimal base (map digitToInt str)

-- Converts a string in a given base to decimal using list comprehension and `zip`
decimal :: Int -> String -> Int
decimal base str = sum [digitToInt x * base^i | (x, i) <- zip (reverse str) [0..]]


--Using zip
binaryToDecimal :: [Int] -> Int
binaryToDecimal bs = sum [x * 2^i | (x, i) <- zip (reverse bs) [0..]]
-- `reverse bs` reverses the list of binary digits so we can process from the least significant to the most significant.
-- `zip (reverse bs) [0..]` pairs each digit with its position index.
-- `[x * 2^i | (x, i) <- zip (reverse bs) [0..]]` calculates the decimal value by summing each digit multiplied by 2 raised to its position index.
-- `sum` adds up all these values to get the final decimal number.


--4. Firsts Elements of a list
firsts :: [a] -> [[a]]
firsts xs = [take n xs | n <- [1..length xs]]
--         [expression | generator, condition] the condition is optional
-- `take n xs` extracts the first `n` elements from `xs`.
-- `[1..length xs]` generates a list of integers from 1 to the length of `xs`.
-- The list comprehension `[take n xs | n <- [1..length xs]]` creates a list of prefixes of increasing length.


--5. Binary Operations – BONUS exercise
-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation



-- Adds two binary strings and returns the result as a binary string
binaryAdd :: String -> String -> String
binaryAdd a b = reverse (addBinary (reverse a) (reverse b) 0)
  where
    -- Recursive function to add binary digits with carry
    addBinary :: String -> String -> Int -> String
    addBinary [] [] 0 = []  -- Both lists empty and no carry
    addBinary [] [] carry = [intToDigit carry]  -- Both lists empty but carry exists
    addBinary (x:xs) [] carry = addBinary xs [] carry'  -- Process remaining digits in first string
      where carry' = (digitToInt x + carry) `div` 2  -- Update carry
    addBinary [] (y:ys) carry = addBinary [] ys carry'  -- Process remaining digits in second string
      where carry' = (digitToInt y + carry) `div` 2  -- Update carry
    addBinary (x:xs) (y:ys) carry =
      let sum = digitToInt x + digitToInt y + carry  -- Sum of digits and carry
          carry' = sum `div` 2  -- Calculate new carry
          digit = sum `mod` 2  -- Calculate binary digit
      in intToDigit digit : addBinary xs ys carry'  -- Build result

    -- Converts an integer (0 or 1) to a binary character
    intToDigit :: Int -> Char
    intToDigit 0 = '0'
    intToDigit 1 = '1'
    intToDigit _ = error "Only binary digits are supported"


