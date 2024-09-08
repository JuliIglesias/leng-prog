module Huffman (huffmanTrie, encode, decode, Trie(..), Bit(..)) where

import qualified Data.Map as M
import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

frequencyMap :: (Ord a) => [a] -> M.Map a Int
frequencyMap = foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty

data Bit = F | T deriving (Eq, Show)
type Bits = [Bit]

data Trie a = Empty
            | Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

huffmanTrie :: String -> Trie Char
huffmanTrie "" = Empty
huffmanTrie input = buildTrie $ map (\(c, f) -> (f, Leaf c)) $ M.toList $ frequencyMap input
  where
    buildTrie [(_, trie)] = trie
    buildTrie ((f1, t1):(f2, t2):rest) = buildTrie $ insertBy (comparing fst) (f1 + f2, t1 :-: t2) rest
    buildTrie [] = Empty

encode :: String -> Trie Char -> Bits
encode input trie = concatMap (charToBits M.!) input
  where
    charToBits = buildMap trie M.empty []
    buildMap (Leaf c) m bits = M.insert c bits m
    buildMap (l :-: r) m bits = buildMap r (buildMap l m (bits ++ [F])) (bits ++ [T])
    buildMap Empty m _ = m

decode :: Bits -> Trie Char -> String
decode bits trie = decodeBits bits trie
  where
    decodeBits [] _ = []
    decodeBits bs t = case decodeChar bs t of
                        (c, rest) -> c : decodeBits rest t
    decodeChar (F:bs) (l :-: _) = decodeChar bs l
    decodeChar (T:bs) (_ :-: r) = decodeChar bs r
    decodeChar [] (Leaf c) = (c, [])
    decodeChar bs (Leaf c) = (c, bs)
    decodeChar _ Empty = error "Invalid Trie structure"