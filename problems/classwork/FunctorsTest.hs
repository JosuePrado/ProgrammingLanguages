module FunctorsTest where

    data BinaryTree a = Empty
                        | Node a (BinaryTree a) (BinaryTree a)
                        deriving (Show)

    instance Functor BinaryTree where 
        fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
        fmap _ Empty = Empty
        fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)

    instance Applicative BinaryTree where
        pure :: a -> BinaryTree a
        pure x = Node x Empty Empty
        (<*>) :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
        Empty <*> _ = Empty
        _ <*> Empty = Empty
        (Node f leftF rightF) <*> (Node x leftX rightX) = Node (f x) (leftF <*> leftX) (rightF <*> rightX)    

    
    incrementValue :: Num a => BinaryTree a -> BinaryTree a 
    incrementValue Empty = Empty
    incrementValue tree = fmap (+1) tree

    example1 = Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)

    stringLength :: BinaryTree String -> BinaryTree Int 
    stringLength Empty = Empty
    stringLength tree = fmap length tree

    example2 = Node "a" (Node "bc" Empty Empty) (Node "efg" Empty Empty)

    mergeTrees :: BinaryTree a -> BinaryTree a -> BinaryTree a
    mergeTrees tree1 Empty = tree1
    mergeTrees Empty tree2 = tree2
    mergeTrees (Node x leftX rightX) (Node y leftY rightY) = 
    