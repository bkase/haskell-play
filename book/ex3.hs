module Ex3 where

thirdLetter :: [a] -> a
thirdLetter x = x !! 2

rev3 :: [a] -> [a]
rev3 str = third ++ second ++ first
  where first = take 5 str
        second = take 2 $ drop 5 str
        third = drop 8 str

main :: IO ()
main = print $ rev3 "Curry is awesome"
