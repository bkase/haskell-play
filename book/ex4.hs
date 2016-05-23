module Ex4 where

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

x :: Int -> Int -> Int
x = (+)

f :: Foldable t => t a -> Int
f xs = w `x` 1
  where w = length xs

