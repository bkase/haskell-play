module Ex9 where

eft :: (Eq a, Enum a) => a -> a -> [a]
eft from to = reverse $ go from to []
  where go from to build
         | from == to = build
         | otherwise = go (succ from) to (from : build)

-- Did anyone do this better?
myWords :: String -> Char -> [String]
myWords sentence sep = reverse $ go sentence []
  where go s build
         | s == "" = build
         | otherwise = go rest (word : build)
         where word = takeWhile (/=sep) s
               rest = safeTail $ dropWhile (/=sep) s
               safeTail [] = []
               safeTail (_ : xs) = xs


