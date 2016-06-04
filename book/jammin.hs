module Jammin where

import Data.List

data Fruit =
      Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Show, Ord)

-- cardinality = 4 * sizeof(Int)
data JamJars = JamJars { fruit :: Fruit
                       , count :: Int }
                       deriving (Eq, Show, Ord)

row1,row2,row3 :: [JamJars]
row1 = [ JamJars Peach 5, JamJars Plum 4 ]
row2 = [ JamJars Apple 2, JamJars Blackberry 1 ]
row3 = [ JamJars Peach 1 ]

allRows :: [JamJars]
allRows = concat [row1, row2, row3]

totalJars :: [JamJars] -> Int
totalJars = sum . fmap count

-- TODO: Inelegant
mostRow :: [JamJars] -> JamJars
mostRow = foldr (\a -> \b -> if (max (count a) (count b)) == count a then a else b) (JamJars Peach 0)

sortJars :: [JamJars] -> [JamJars]
sortJars = sortBy compareKind
  where compareKind (JamJars k _) (JamJars k' _) = compare k k'

groupJars :: [JamJars] -> [[JamJars]]
groupJars = groupBy kindEq . sortJars
  where kindEq (JamJars k _) (JamJars k' _) = k == k'

type Gardener = String
-- normal form
data Garden =
    Gardenia Gardener 
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener

data GuessWhat = ChickenButt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

data OperatingSystem = Mac | Linux | Windows deriving (Eq, Show)
data ProgrammingLang = Haskell | Scala | Python deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLang }
                             deriving (Eq, Show)

allOs :: [OperatingSystem]
allOs =
  [ Linux
  , Mac
  , Windows
  ]

allLangs :: [ProgrammingLang]
allLangs =
  [ Haskell
  , Scala
  , Python
  ]

allProgrammers :: [Programmer]
allProgrammers = do
  os <- allOs
  lang <- allLangs
  return $ Programmer os lang

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left v right)
  | b == v = Node left v right
  | b < v = Node (insert' b left) v right
  | b > v = Node left v (insert' b right)

instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node left v right) = Node (f <$> left) (f v) (f <$> right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left v right) = v : (preorder left ++ preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left v right) = (inorder left) ++ [v] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left v right) = (postorder left) ++ (postorder right) ++ [v]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTreeOp :: (BinaryTree Integer -> [Integer]) -> String -> [Integer] -> IO ()
testTreeOp op name expect =
  if op testTree == expect
  then putStrLn "good"
  else putStrLn $ "bad " ++ name

testPreorder :: IO ()
testPreorder = testTreeOp preorder "preorder" [2, 1, 3]

testInorder :: IO ()
testInorder = testTreeOp inorder "inorder" [1, 2, 3]

testPostorder :: IO ()
testPostorder = testTreeOp postorder "postorder" [1, 3, 2]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z = foldr f z . inorder

-- I think it's impossible to maintain structure when you right map in terms of fold with no Ord
{-
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f = foldTree (makeReducer f) Leaf
  where makeReducer :: (a -> b) -> BinaryTree a -> BinaryTree b -> BinaryTree b
        makeReducer f Leaf Leaf = Leaf
        makeReducer f a (Node left v right) = Node left (f v) right
-}

