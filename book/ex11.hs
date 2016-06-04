module Ex11 where

{-# LANGUAGE GeneralizedNewTypeDeriving #-}

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (< 42)

newtype IntStr = IntStr (Int, String) deriving (Eq, Show)
instance TooMany IntStr where
  tooMany (IntStr (n, _)) = n < 42

newtype Aa a = Aa (a, a)
instance (Num a, TooMany a) => TooMany (Aa a) where
  tooMany (Aa (x, y)) = tooMany $ x + y
