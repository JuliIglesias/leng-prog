module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens) where

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F 

charToBits :: Char -> Bits
charToBits char = [bitAt n char | n <- [0..7]]

bits::String -> Bits
bits str = concat [charToBits c | c <- str]


type Solution = [Int]

-- el ROW se va sumando hasta coincidir con el n+1
-- el COL es un alista con la columna q usa
-- Diagonal es row - col
-- AntiDiagonal es row + col

--Necesitamos: recursion entre soluciones y el tablero
queens::Int -> [Solution]
queens n = solve 1 [[]]
  where solve row solutions
  | row > n = solutions
  | otherwise = solve (row+1) [col:solution | col <- [1..n], solution <- solutions, ok col 1 solution]
  ok _ _ [] = True
  ok col offset (c:cs) = col /= c && col /= c-offset && col/= c + offset && ok col (offset+1) cs