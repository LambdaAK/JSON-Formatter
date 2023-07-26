module Proc (
    Proc(..)
)
where

data Proc a =
    Err String | Suc a deriving (Show)

instance Functor Proc where
    fmap _ (Err e) = Err e
    fmap f (Suc v) = Suc (f v)

instance Applicative Proc where
    pure = Suc
    (Err e) <*> _ = Err e
    (Suc f) <*> v = fmap f v

instance Monad Proc where
    return = pure
    (Err e) >>= _ = Err e
    (Suc v) >>= f = f v