module CollatzConjecture (collatz) where

countSteps :: Integer -> Integer
countSteps c = c + 1

evenNumber :: Integer -> Integer
evenNumber c = c `div` 2

oddNumber :: Integer -> Integer
oddNumber c = 3 * c + 1



collatz :: Integer -> Maybe Integer
collatz n = rst
  where evenNum = evenNumber n
        oddNum = oddNumber n
        rst = if n <= 0 then Nothing
              else if n == 1 then Just 0
              else if even n then case collatz evenNum of
                                    Nothing -> Nothing
                                    Just steps -> Just (countSteps steps)
              else case collatz oddNum of
                     Nothing -> Nothing
                     Just steps -> Just (countSteps steps)
