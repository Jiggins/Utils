module CountingSet where

import Data.List (intercalate)

type Count = Int
type Size  = Int

data Set a = Tree {-# UNPACK #-} !a !Size !Count !(Set a) !(Set a)
           | Leaf deriving (Show)

null :: Set a -> Bool
null Leaf = True
null _   = False

size :: Set a -> Size
size Leaf = 0
size (Tree _ s _ _ _) = s

count :: Set a -> Count
count Leaf = 0
count (Tree _ _ c _ _) = c

left :: Set a -> Set a
left Leaf = Leaf
left (Tree _ _ _ l _) = l

right :: Set a -> Set a
right Leaf = Leaf
right (Tree _ _ _ _ r) = r

singleton :: a -> Set a
singleton x = Tree x 0 1 Leaf Leaf

insert :: Ord a => a -> Set a -> Set a
insert a Leaf = Tree a 1 1 Leaf Leaf
insert a (Tree x s c left right) = case compare a x of
    LT -> balance x c (insert a left) right
    GT -> balance x c left (insert a right)
    EQ -> Tree x s (c+1) left right

find :: Ord a => a -> Set a -> Maybe (Set a)
find _ Leaf = Nothing
find a set@(Tree x s c left right) = case a `compare` x of
    LT -> find a left
    GT -> find a right
    EQ -> Just set

amountOf :: Ord a => a -> Set a -> Maybe Count
amountOf a = fmap count . find a

sizeOfTreeAt :: Ord a => a -> Set a -> Maybe Size
sizeOfTreeAt a = fmap size . find a

delta, ratio :: Int
delta = 3
ratio = 2

showTree (Tree a c s left right) = show a ++ "\n" ++ (showTree left) ++ "\t" ++ (showTree right)

balance :: a -> Count -> Set a -> Set a -> Set a
balance x c l r
    | sizeL + sizeR <= 1   = Tree x sizeX c l r
    | sizeR > delta*sizeL  = rotateL x sizeX c l r
    | sizeL > delta*sizeR  = rotateR x sizeX c l r
    | otherwise            = Tree x sizeX c l r
    where
      sizeL = size l
      sizeR = size r
      sizeX = sizeL + sizeR + 1

rotateL :: a -> Size -> Count -> Set a -> Set a -> Set a
rotateL x s c l r@(Tree _ _ _ ly ry)
    | size ly < ratio*size ry = singleL x s c l r
    | otherwise               = doubleL x s c l r

rotateR :: a -> Size -> Count -> Set a -> Set a -> Set a
rotateR x s c l@(Tree _ _ _ ly ry) r
    | size ry < ratio*size ly = singleR x s c l r
    | otherwise               = doubleR x s c l r

singleL, singleR :: a -> Size -> Count -> Set a -> Set a -> Set a
singleL a s c left (Tree x' s' c' left' right')  = Tree x' s' c' (Tree a s c left left') right'
singleR a s c (Tree x' s' c' left' right') right = Tree x' s' c' left' (Tree a s c right' right)

doubleL, doubleR :: a -> Size -> Count -> Set a -> Set a -> Set a
doubleL a s c left (Tree x' s' c' (Tree x'' s'' c'' l r) right') = Tree x'' s'' c'' (Tree a s c left l) (Tree x' s' c' r right')
doubleR a s c (Tree x' s' c' left' (Tree x'' s'' c'' l r)) right = Tree x'' s'' c'' (Tree x' s' c' left' l) (Tree a s c r right)
