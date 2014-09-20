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

{- * Accessors -}

left :: Ord a => Tree a -> Tree a
left (Node _ _ left _) = left

right :: Ord a => Tree a -> Tree a
right (Node _ _ _ right) = right

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

insertVal :: Ord a => a -> Tree a -> Tree a
insertVal = insert . node

insert :: Ord a => Tree a -> Tree a -> Tree a
insert new Empty = new
insert new tree
    | value new > value tree = insert new (right tree)
    | value new < value tree = insert new (left tree)
    | otherwise = tree -- Empty tree becomes the new tree