module ListsPart2 (Bit(..), bitAt, charToBits, bits) where
--, queens

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F 

charToBits :: Char -> Bits
charToBits c = [bitAt n c | n <- [0..7]]
-- calls bitAt 8 times, one for each bit
-- creates a list of 8 bits



bits::String -> Bits
bits str = foldr (++) [] (map charToBits str)
-- `map charToBits str` converts each character in the string to a list of bits.
-- `foldr (++) []` concatenates all these lists of bits into a single list of bits.

-- BONUS exercise: TODO
{-
type Solution = [Int]

queens::Int -> [Solution]
queens error "Implement It"
-}