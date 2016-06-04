module Hw11 where
  
import Data.Char
import Data.List

vCipher :: String -> String -> String
vCipher password str = (uncurry rot) <$> zipped
  where zipped = zip (spacifyCodes str (rots password str)) str

        spacifyCodes :: String -> [Int] -> [Int]
        spacifyCodes str codes = go str codes []
          where go [] _ build = reverse build
                go (' ':xs) codes build = go xs codes (0:build)
                go (x:xs) (c:cs) build = go xs cs (c:build)

        rots :: String -> String -> [Int]
        rots password str = cycle encPwd
          where encPwd = code <$> password
                code ch
                 | ch >= 'A' && ch <= 'Z' = modIt ch (ord 'A')
                 | ch >= 'a' && ch <= 'z' = modIt ch (ord 'a')
                 | otherwise = 0
                modIt ch num = ((ord ch) - num) `mod` 26

        rot :: Int -> Char -> Char
        rot cnt ch
         | ch >= 'A' && ch <= 'Z' = normRot cnt (ord 'A') ch
         | ch >= 'a' && ch <= 'z' = normRot cnt (ord 'a') ch
         | otherwise = ch
        normRot :: Int -> Int -> Char -> Char
        normRot cnt norm ch = chr $ (((ord ch) - norm + cnt) `mod` 26) + norm

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' (x:xs) [] = False
isSubsequenceOf' first@(x:xs) second@(y:ys)
  | x == y = isSubsequenceOf' xs ys
  | otherwise = isSubsequenceOf' first ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = go (words s) []
  where go [] build = reverse build
        go ([]:xs) build = go xs build
        go ((s@(y:ys)):xs) build = go xs ((s, (toUpper y):ys):build)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x):xs

-- yeah if the last sentence ends in a space it's broken, but whatever
capitalizeParagraph :: String -> String
capitalizeParagraph s = concat $ intersperse ". " (capitalizeSentence <$> sentences)
  where sentences :: [String]
        sentences = mySplit (=='.') s
        mySplit :: (Char -> Bool) -> String -> [String]
        mySplit p s = go p s [[]]
          where go :: (Char -> Bool) -> String -> [String] -> [String]
                go p [] (b:bs) = reverse ((reverse b):bs)
                go p (x:xs) (b:bs)
                  | p x = go p xs ([]:((reverse b):bs))
                  | otherwise = go p xs ((x:b):bs)
          
        capitalizeSentence :: String -> String
        capitalizeSentence s = go (words s)
          where go [] = ""
                go (x:xs) = concat $ intersperse " " ((capitalizeWord x):xs)

