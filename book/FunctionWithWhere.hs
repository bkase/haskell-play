-- FunctionWithWhere.hs

module FunctionWithWhere where

printInc :: (Num a, Show a) => a -> IO ()
printInc n = let plusTwo = n + 2
              in print plusTwo

mult1 :: Integer
mult1 = x * y
  where x = 5
        y = 6

waxOn :: Integer
waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple :: Num a => a -> a
triple x = x * 3

y :: Integer
y = 5

waxOff :: Num a => a -> a
waxOff x = triple x

