module Hw10 where

import Control.Monad
import Control.Applicative

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: (Monad m, Alternative m, Eq a) => m a -> m a -> Maybe a -> m (a, a, a)
combos c v constraint = do
  stop1 <- c
  guard $ maybe True (stop1==) constraint
  vowel <- v
  stop2 <- c
  return (stop1, vowel, stop2)

avgWordLen :: Fractional a => String -> a
avgWordLen ws = allLens / (count ws)
  where allLens = fromIntegral . sum $ length <$> (words ws)
        count = fromIntegral . length . words

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

