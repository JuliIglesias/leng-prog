module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

numerator :: Fraction -> Int
numerator (n,_) = n

denominator :: Fraction -> Int
denominator (_,d) = d

-- Helper function to convert two fractions to a common denominator
toCommonDenominator :: Fraction -> Fraction -> (Fraction, Fraction)
toCommonDenominator frac1 frac2 = ( ( n1 * d2 , d1 * d2 ) , ( n2 * d1 , d1 * d2 ) )
  where n1 = numerator frac1
        d1 = denominator frac1
        n2 = numerator frac2
        d2 = denominator frac2




-- Implement the `add` Function
add :: Fraction -> Fraction -> Fraction
add n d = simplify rst
  where common = toCommonDenominator n d
        num1 = numerator $ fst common
        num2 = numerator $ snd common
        den1 = denominator $ fst common
        rst = (num1 + num2, den1)


-- Implement the `sub` Function
sub :: Fraction -> Fraction -> Fraction
sub n d = simplify rst
      where common = toCommonDenominator n d
            num1 = numerator $ fst common
            num2 = numerator $ snd common
            den1 = denominator $ fst common
            rst = (num1 - num2, den1)

-- Implement the `mul` Function
mul :: Fraction -> Fraction -> Fraction
mul n d = simplify rst
      where num1 = numerator n
            num2 = numerator d
            den1 = denominator n
            den2 = denominator d
            rst = (num1 * num2, den1 * den2)

-- Implement the `divide` Function
divide :: Fraction -> Fraction -> Fraction
divide n d = simplify rst
         where num1 = numerator n
               num2 = denominator d
               den1 = denominator n
               den2 = numerator d
               rst = (num1 * num2, den1 * den2)

-- Implement the `hcf` Function using the Euclidean algorithm
hcf :: Int -> Int -> Int
hcf a 0 = a
hcf a b = hcf b (a `mod` b)

-- Implement the `simplify` Function
simplify :: Fraction -> Fraction
simplify (n, d) = (n `div` h, d `div` h)
  where h = hcf n d




    