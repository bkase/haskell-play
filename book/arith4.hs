module Arith4 where

roundTrip :: (Read a, Show a) => a -> a
roundTrip = read . show

roundTrip2 :: (Read a, Show b) => b -> a
roundTrip2 = read . show

main :: IO ()
main = do
  print $ roundTrip (4 :: Int)
  print $ id 4
