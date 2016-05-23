module Ex8 where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n-d) d (count+1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumN :: (Eq a, Num a) => a -> a
sumN n = go n 0
  where go n build
         | n == 0 = build
         | otherwise = go (n-1) (build+n)

multN :: Integral a => a -> a -> a
multN x y = go x y 0
  where go x y build
         | x == 0 = build
         | otherwise = go (x-1) y (build+y)

dividedByRedux :: Integral a => a -> a -> Maybe (a, a)
dividedByRedux num denom
  | denom == 0 = Nothing
  | otherwise = Just $ let (res, rem) = dividedBy (abs num) (abs denom) in 
                           ((sign num) * (sign denom) * res, rem)
    where 
      sign :: (Num a, Ord a) => a -> a
      sign x = if x < 0 then -1 else 1

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91(mc91(n+11))

