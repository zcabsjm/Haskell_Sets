module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type, or 
  wrap a binary search tree.
  Extra marks will be awarded for efficient implementations if appropriate.

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
newtype Set a = Set { unSet :: [a] } deriving (Show)

{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList s1 =  (unSet s1)

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

unique :: Ord a => [a] -> [a] -> [a]
unique [] acc = acc
unique (x:xs) acc
  | elem x acc = unique xs acc
  | otherwise  = unique xs (x:acc)

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList xs = Set (quicksort (unique xs []))

{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1) == (toList s2)


-- the empty set
empty :: Set a
empty = Set []


-- Set with one element
singleton :: a -> Set a
singleton x = Set [x]


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert n set
  | elem n (unSet set) = set
  | otherwise          = Set (n:(unSet set))


-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = union' s1 (unSet s2)

union' :: (Ord a) => Set a -> [a] -> Set a
union' s1 [] = s1
union' s1 (x:xs) = union' (insert x s1) xs


-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = Set(intersection' (unSet s1) (unSet s2) [])

intersection' :: (Ord a) => [a] -> [a] -> [a] -> [a]
intersection' s1 [] acc = acc
intersection' s1 (x:xs) acc
  | elem x s1 = intersection' s1 xs (x:acc)
  | otherwise = intersection' s1 xs acc


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = Set(difference' (unSet s1) (unSet s2) [])

difference' :: (Ord a) => [a] -> [a] -> [a] -> [a]
difference' [] s2 acc = acc
difference' (x:xs) s2 acc
  | elem x s2 = difference' xs s2 acc
  | otherwise = difference' xs s2 (x:acc)

-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member e s = elem e (unSet s)


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality s = length (unSet s)


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f s = Set (map f (unSet s))


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f s v = foldr f v (unSet s)


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet s = Set ( map fromList (powerset' (unSet s)) )

powerset' :: [a] -> [[a]]
powerset' [] = [[]]
powerset' (x:xs) = [x:ps | ps <- powerset' xs] ++ powerset' xs  


-- cartesian product of two sets
cartesian :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cartesian s1 s2 = fromList [(x, y) | x <- (unSet s1), y <- (unSet s2)]


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (Ord a) => (a -> Bool) -> Set a -> (Set a, Set a)
partition f s = (fromList left, fromList right)
  where
    (left, right) = partition' f (unSet s) [] [] 

partition' :: (a -> Bool) -> [a] -> [a] ->[a] -> ([a], [a])
partition' f [] left right = (left, right)
partition' f (x:xs) left right
  | f x       = partition' f xs (x:left) right
  | otherwise = partition' f xs left (x:right)

{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

-}
