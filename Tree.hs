-- | Haskell implementation of Red Black Binary trees
module Tree where

import Data.Function
import Utils ((.:))

data Colour = Red | Black deriving (Eq, Show)

data Tree a = Node a Colour (Tree a) (Tree a)
            | Empty
            deriving (Eq, Show)

-- | Allow for Nodes to be compared by their value
instance Ord a => Ord (Tree a) where
    compare = compare `on` value

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x c left right) = Node (f x) c (fmap f left) (fmap f right) 

{- * Accessors -}

getLeft :: Ord a => Tree a -> Tree a
getLeft (Node _ _ left _) = left

getRight :: Ord a => Tree a -> Tree a
getRight (Node _ _ _ right) = right

value :: Ord a => Tree a -> a
value (Node value _ _ _) = value

colour :: Ord a => Tree a -> Colour
colour (Node _ colour _ _) = colour

-- | Check if two trees have the same colour.
sameColour :: Ord a => Tree a -> Tree a -> Bool
sameColour = (==) `on` colour

-- | Check if two trees have different colours.
differentColour :: Ord a => Tree a -> Tree a -> Bool
differentColour = not .: sameColour

{- * Constructors -}

-- | Creates a new red node.  New nodes are Red by default.
node :: Ord a => a -> Tree a
node x = Node x Red Empty Empty

-- | Converts a node to Red
makeRed :: Ord a => Tree a -> Tree a
makeRed (Node x _ left right) = Node x Red left right

-- | Converts a node to Black
makeBlack :: Ord a => Tree a -> Tree a
makeBlack (Node x _ left right) = Node x Black left right

{- * Insertions -}

insert :: Ord a => a -> Tree a -> Tree a
insert node Empty = Node node Red Empty Empty
insert node (Node a c left right) = 
    case node `compare` a of
        LT -> Node a Red (insert node left) right
        GT -> Node a Red left (insert node right)
        EQ -> Node a c left right

search :: Ord a => a -> Tree a -> Maybe a
search _ Empty = Nothing
search x tree = case x `compare` value tree of
    LT -> search x (getLeft tree)
    EQ -> Just x
    GT -> search x (getRight tree)

main = do
    let tree = Empty
    print . insert 3 . insert 5 . insert 2 . insert 4 $ tree
    print Red