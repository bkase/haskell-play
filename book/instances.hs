module Instances where

data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
  (==) (Pair x y) (Pair a b) = (x == a) && (y == b)
