module NumbersToWords where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "oops"

digits :: Int -> [Int]
digits x = go x []
  where go x build
         | x == 0 = build
         | otherwise = let (d, m) = x `divMod` 10 in go d (m : build)

wordNumber n = mconcat (intersperse "-" $ words n)
  where words n = digitToWord <$> (digits n)

