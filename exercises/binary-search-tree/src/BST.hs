module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Leaf | Node (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf = Nothing
bstLeft (Node left _ _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf = Nothing
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Leaf = Nothing
bstValue (Node _ value _) = Just value

empty :: BST a
empty = Leaf

fromReversedList :: Ord a => [a] -> BST a
fromReversedList [] = empty
fromReversedList [x] = singleton x
fromReversedList (x:xs) = insert x (fromReversedList xs)

fromList :: Ord a => [a] -> BST a
fromList xs = fromReversedList (reverse xs)

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node left value right)
    | x <= value = Node (insert x left) value right
    | otherwise = Node left value (insert x right)

singleton :: a -> BST a
singleton x = Node Leaf x Leaf

toList :: BST a -> [a]
toList Leaf = []
toList (Node left value right) = toList left ++ [value] ++ toList right
