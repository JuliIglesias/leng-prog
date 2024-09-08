module Frequencies  (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)

type Frequency = (Int, Char)

frequencies :: String -> [Frequency]
frequencies str = insertionSort $ map swap $ Map.toList $ frequencyMap str

frequencyMap :: (Ord a) => [a] -> Map a Int
frequencyMap = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

-- Sort a list using insertion sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []

-- Chau jli :D atte. Eze Quiel

import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (sortBy)
import Data.Ord (comparing)
import Frequencies (frequencies)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: No file name provided."
    else do
      let fileName = head args
      content <- readFile fileName
      let freqList = frequencies content
      let sortedFreqList = reverse $ sortBy (comparing fst) freqList
      mapM_ (\(count, char) -> putStrLn $ show char ++ ": " ++ show count) sortedFreqList