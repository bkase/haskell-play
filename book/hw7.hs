module Hw7 where

digits :: Integer -> [Integer]
digits x = helper x []
  where 
    helper x build =
      if x == 0 then build else 
        let (d, m) = x `divMod` 10 in helper d (m : build)
        
tensD :: Integer -> Integer
tensD x = head $ f x
  where f = tail . take 2 . reverse . digits

hunsD :: Integer -> Integer
hunsD x = head $ f x
  where f = tail . tail . take 3 . reverse . digits

foldBool :: a -> a -> Bool -> a
foldBool x y b = if b then x else y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b = x
  | otherwise = y

g :: (t -> t1) -> (t, t2) -> (t1, t2)
g f (a, c) = (f a, c)

