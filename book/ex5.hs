module Ex5 where

funcIgnoresArgs :: t -> t1 -> t2 -> String
funcIgnoresArgs x y z = "hello"

nonsense :: Num a => Bool -> a
nonsense True = 805
nonsense False = 100

typical :: Integer -> Bool -> Integer
typical i b = (i+) $ nonsense b

uncurried :: (Integer, Bool) -> Integer
uncurried (i, b) = (i+) $ nonsense b

anon :: Integer -> Bool -> Integer
anon = \i b -> (i+) $ nonsense b

anonNest :: Integer -> Bool -> Integer
anonNest = \i -> \b -> (i+) $ nonsense b

