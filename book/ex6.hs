module Ex6 where

divideAndMod :: Fractional a => a -> a -> a
divideAndMod x y = (x / y) + 1

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = (Age n)
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = (Year n)
  toNumber (Year n) = n
  defaultNumber = Year 1998

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where iOfA = toNumber a
        iOfA' = toNumber a'
        summed = iOfA + iOfA'


data Trivial = Trivial

instance Eq Trivial where
  Trivial == Trivial = True


data DayOfWeek =
  Mon | Tues | Weds | Thur | Fri | Sat | Sun

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tues Tues = True
  (==) Weds Weds = True
  (==) Thur Thur = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date d month) (Date d' month') = d == d' && month == month'

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt = TInt Int | TString String
instance Eq StringOrInt where
  (==) (TInt x) (TInt y) = x == y
  (==) (TString x) (TString y) = x == y
  (==) _ _ = False

data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = This a | That a
instance (Eq a) => Eq (Which a) where
  (==) (This x) (This y) = x == y
  (==) (That x) (That y) = x == y
  (==) _ _ = False

data EitherOr a b = L a | R b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (L x) (L y) = x == y
  (==) (R x) (R y) = x == y
  (==) _ _ = False

i :: Num a => a
i = 1

f :: Float 
f = 1.0


