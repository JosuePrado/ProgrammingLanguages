module Functors where

    import Control.Applicative
    
    data MyList a = Empty
        | Cons a (MyList a)
        deriving (Show, Eq, Ord)

    instance Functor MyList where 
        fmap:: (a -> b) -> MyList a -> MyList b
        fmap _ Empty = Empty
        fmap f (Cons a xs) = Cons (f a) (fmap f xs)

    instance Applicative MyList where 
        pure :: a -> MyList a 
        pure a = Cons a Empty

        (<*>) :: MyList (a -> b) -> MyList a -> MyList b
        (<*>) _ Empty = Empty

    ex = [(+), (-)] <*> [1,2] <*> [3,4]
        


    data TrafficLight = Red | Yellow | Green

    instance Eq TrafficLight where 
        (==) :: TrafficLight -> TrafficLight -> Bool
        Red == Red = True 
        Yellow == Yellow = True 
        Green == Green = True 
        _ == _ = False 
        (/=) :: TrafficLight -> TrafficLight -> Bool
        x /= y = not (x == y) 

    data CMaybe a = CNothing 
                | CJust Int a 
                deriving (Show)

    instance Functor CMaybe where 
        fmap :: (a -> b) -> CMaybe a -> CMaybe b
        fmap _ CNothing = CNothing
        fmap f (CJust x y) = CJust (x + 1) (f y)

    x = fmap (++ "haaa") (CJust 0 "ho")
    y = fmap (++ "haaa") (CJust 1 "ho")

    data BinaryTree a = BEmpty
                    | Node a (BinaryTree a) (BinaryTree a)
                    deriving (Show)

    instance Functor BinaryTree where 
        fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
        fmap _ BEmpty = BEmpty
        fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)

    instance Applicative BinaryTree where
        pure :: a -> BinaryTree a
        pure x = Node x BEmpty BEmpty
        (<*>) :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
        BEmpty <*> _ = BEmpty
        _ <*> BEmpty = BEmpty
        (Node f leftF rightF) <*> (Node x leftX rightX) = Node (f x) (leftF <*> leftX) (rightF <*> rightX)    


    addMaybes :: Maybe Int -> Maybe Int -> Maybe Int 
    addMaybes mx my = (+) <$> mx <*> my

    -- Alternative Functors <|>

    instance Alternative MyList where 
        empty :: MyList a
        empty = Empty 
        (<|>) :: MyList a -> MyList a -> MyList a
        Empty <|> xs = xs
        xs <|> Empty = xs 
        (Cons x xs) <|> ys = Cons x (xs <|> ys)

    example :: Maybe Int 
    example = Just 1 <|> Just 2
