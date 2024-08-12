module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal,
              firsts,
              binaryAdd) where
  
import Data.Char(digitToInt)  

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
intersection [] ys = []
intersection (x:xs) ys
  | member x ys = x : intersection xs ys
  | otherwise = intersection xs ys

difference:: [Int] -> [Int] -> [Int]
difference [] ys = [] -- no entiendo porque no se puede poner al reves pero funca
difference (x:xs) ys
  | member x ys = difference xs ys
  | otherwise = x : difference xs ys

insert:: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x:xs)
  | a <= x = a : x : xs
  | otherwise = x : insert a xs

--se podria hacer con foldr o foldl
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x:xs) = x * 2 ^ length xs + binaryToDecimal xs
    
toDecimal :: Int -> [Int] -> Int
toDecimal base [] = 0
toDecimal base (x:xs) = x * base ^ length xs + toDecimal base xs
    
toDec::Int -> String -> Int
toDec base s = toDecimal base (map digitToInt s)

-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal base s = sum [digitToInt x * base ^ i | (x, i) <- zip s [length s - 1, length s - 2..0]]

--Given a List, return a Nested List containing:
--The first element, the first 2 elements, the first 3 elements, etc.
firsts::[a] -> [[a]]
firsts [] = []
firsts xs = [take i xs | i <- [1.. (length xs)]]

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd  = error "Implement it"