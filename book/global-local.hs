module GlobalLocal where

topLevelFun :: Integer -> Integer
topLevelFun x = x + woot + topLevelValue
  where woot :: Integer
        woot = 5

topLevelValue :: Integer
topLevelValue = 5


