module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit

-- El operador :-: es un constructor de datos definido en el tipo Trie en Haskell.
-- Se utiliza para combinar dos subárboles en un solo nodo.
data Trie a = Leaf a | Trie a :-: Trie a deriving (Eq, Show)
-- data Leaf no hace falta definirla xq es como q ya la definis en el TRIE

-- La función leaf toma un valor y devuelve un Trie que contiene ese valor.
leaf::Show a => a -> String
leaf a = show a

-- La función left toma un Trie y devuelve el subárbol izquierdo.
-- si quiero el error debo de agregarle esto a left y right: Show a =>, como leaf arriba
left::Show a => Trie a -> Trie a
left (left :-: _) = left
left (Leaf a) = error $ "Left of: Leaf '" ++ (leaf a) ++ "'"

-- La función right toma un Trie y devuelve el subárbol derecho.
right::Show a => Trie a -> Trie a
right (_ :-: right) = right
right (Leaf a) = error $ "Right of: Leaf '" ++ (show a) ++ "'"

-- La función find toma una lista de bits y un Trie y devuelve el valor almacenado en el Trie que corresponde a la lista de bits.
find::Bits -> Trie a -> a
find bits (left :-: right) = find (tail bits) (trie)
  where trie = if head bits == T then right else left

-- La función decode toma una lista de bits y un Trie y devuelve la cadena de caracteres que corresponde a la lista de bits.decode :: Bits -> Trie Char -> String
decode :: Bits -> Trie Char -> String

decode bits trie = decode' bits trie
  where
    decode' [] (Leaf c) = c : []
    decode' bs (Leaf c) = c : decode' bs trie
    decode' (b:bs) (left :-: right) = decode' bs (if b == F then left else right)

-- La función toList toma un Trie y devuelve una lista de tuplas que contienen los valores almacenados en el Trie y los bits que los representan.
toList::Trie a -> [(a, Bits)]
toList trie = toTuple trie []
  where toTuple (Leaf a) bits = [(a, bits)]
        toTuple (left :-: right) bits = (toTuple left (bits ++ [F])) ++ (toTuple right (bits ++ [T]))
