module Hw9 where

import Data.Char
import Data.Time

extractUpper :: String -> String
extractUpper = filter isUpper

capFirst :: String -> String
capFirst s = firstCaps s : drop 1 s
  where firstCaps = toUpper . head

capAll :: String -> String
capAll = map toUpper

myOr :: [Bool] -> Bool
myOr = foldl (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny (==e)

myReverse :: [a] -> [a]
myReverse l = go l []
  where go [] build = build
        go (x:xs) build = go xs (x:build)

squish :: [[t]] -> [t]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[b]] -> [b]
squishAgain = squishMap id

myOrderingBy :: Foldable t => Ordering -> (a -> a -> Ordering) -> t a -> a
myOrderingBy o cmp = foldr1 takeBigger
  where takeBigger b x
         | cmp b x == o = b
         | otherwise = x

myMaximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
myMaximumBy = myOrderingBy GT

myMinimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
myMinimumBy = myOrderingBy LT

myMax :: (Ord a, Foldable t) => t a -> a
myMax = myMaximumBy compare

myMin :: (Ord a, Foldable t) => t a -> a
myMin = myMinimumBy compare

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9001
  , DbString "Hello World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = (=<<) keep
  where keep (DbDate d) = [d]
        keep _ = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = (=<<) keep
  where keep (DbNumber n) = [n]
        keep _ = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = last . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = mean . filterDbNumber
  where mean is = let l = fmap fromIntegral is in
                      sum l / length l
