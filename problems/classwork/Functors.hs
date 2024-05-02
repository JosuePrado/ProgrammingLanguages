module Functors where

    data MyList a = Empty
        | Cons a (MyList a)
        deriving (Show, Eq, Ord)
    instance Functor MyList where 
        fmap:: (a -> b) -> MyList a -> MyList b
        fmap _ Empty = Empty
        fmap f (Cons a xs) = Cons (f a) (fmap f xs)

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

    
