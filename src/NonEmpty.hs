{-# LANGUAGE TypeFamilies #-}
module NonEmpty where

import           Data.Foldable
import           Data.Monoid    hiding ((<>))
import           Data.Semigroup

import qualified GHC.Exts       as Exts

data NonEmpty a = End a | Cons a (NonEmpty a) deriving (Eq, Ord, Show)

instance Exts.IsList (NonEmpty a) where
    type Item (NonEmpty a) = a
    fromList [] = error "Non-empty list can't be empty."
    fromList [x] = End x
    fromList (x:xs) = Cons x (Exts.fromList xs)
    toList (End x) = [x]
    toList (Cons x xs) = x : toList xs

instance Functor NonEmpty where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    fmap f (End x) = End $ f x

instance Foldable NonEmpty where
    foldMap f (End x) = f x
    foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Semigroup (NonEmpty a) where
    End x <> xs = Cons x xs
    Cons x xs <> ys = Cons x (xs <> ys)
