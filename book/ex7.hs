module Ex7 where

bindExp :: Integer -> String
bindExp x = let x = 5 in
            let y = 5 in
                show (x + y)

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
                  True -> f n
                  False -> n
                where f = \n -> n + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x -> \y -> (if x > y then y else x) + 5

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

whereItLives :: Penguin -> WherePenguinsLive
whereItLives (Peng w) = w

gentoo :: Penguin
gentoo = Peng Antarctica

functionC :: Ord a => a -> a -> a
functionC x y = case (x > y) of
                  True -> x
                  False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case (even n) of
                 True -> n + 2
                 False -> n

nums :: (Num a, Num a1, Ord a) => a -> a1
nums x =
  case (compare x 0) of
    LT -> -1
    EQ -> 0
    GT -> 1

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise       = False

