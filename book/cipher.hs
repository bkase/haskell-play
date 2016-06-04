module Cipher where

import Data.Char
import Test.QuickCheck.Test

caesar :: Functor f => Int -> f Char -> f Char
caesar cnt = fmap $ rot cnt

rot :: Int -> Char -> Char
rot cnt ch
 | ch >= 'A' && ch <= 'Z' = normRot cnt (ord 'A') ch
 | ch >= 'a' && ch <= 'z' = normRot cnt (ord 'a') ch
 | otherwise = ch
normRot :: Int -> Int -> Char -> Char
normRot cnt norm ch = chr $ (((ord ch) - norm + cnt) `mod` 26) + norm

unCaesar :: Functor f => Int -> f Char -> f Char
unCaesar cnt = caesar (-1 * cnt)

prop_id :: Int -> String -> Bool
prop_id cnt xs = unCaesar cnt (caesar cnt xs) == xs

