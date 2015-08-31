module Probability where

import           Control.Applicative
import           Control.Arrow
import           Data.Foldable
import           Data.List           hiding (all, foldr1, sum)

import           Data.Ratio
import           Data.Semigroup
import           GHC.Exts            (fromList)
import           NonEmpty
import           Positive
import           Prelude             hiding (all, foldr1, sum)

data Prob a = Prob (NonEmpty (Positive, a)) deriving (Eq, Show)

instance Functor Prob where
    fmap f (Prob ps) = Prob $ fmap (second f) ps

instance Applicative Prob where
    pure = return
    Prob fs <*> Prob xs =
        concatP $ fmap (\(p, f) -> distrib p (fmap f (Prob xs))) fs

instance Monad Prob where
    return x = Prob (End (1, x))
    m >>= f  = joinP (fmap f m)

joinP :: Prob (Prob a) -> Prob a
joinP (Prob xs) = concatP (fmap (uncurry distrib) xs)

appendP :: Prob a -> Prob a -> Prob a
appendP (Prob xs) (Prob ys) = Prob $ xs <> ys

concatP :: NonEmpty (Prob a) -> Prob a
concatP = foldr1 appendP

distrib :: Positive -> Prob a -> Prob a
distrib n = mapP (* n)

prob :: [(Positive, a)] -> Prob a
prob = Prob . fromList

choose :: [a] -> Prob a
choose [] = error "No choices"
choose xs = Prob . fromList $ map (\x -> (1, x)) xs

normalize :: Ord a => Prob a -> Prob a
normalize (Prob xs) = Prob . dedupe . fromList $
    sortBy (\(_, x1) (_, x2) -> compare x1 x2) (toList xs)
  where dedupe (End x) = End x
        dedupe (Cons (p1, x1) (End (p2, x2)))
            | x1 == x2 = End (p1 + p2, x1)
            | otherwise = Cons (p1, x1) (End (p2, x2))
        dedupe (Cons (p1, x1) (Cons (p2, x2) ps))
            | x1 == x2 = dedupe $ (p1 + p2, x1) `Cons` ps
            | otherwise = (p1, x1) `Cons` dedupe ((p2,x2) `Cons` ps)

mapP :: (Positive -> Positive) -> Prob a -> Prob a
mapP f (Prob ps) = Prob $ fmap (first f) ps

chance :: Prob Bool -> Double
chance = getTrue . showP . normalize
  where getTrue [] = 0
        getTrue ((n, True):_) = fromRational n
        getTrue (_:xs) = getTrue xs

showP :: Prob t -> [(Rational, t)]
showP (Prob xs) = toList $ fmap (first ((% s) . fromIntegral . fromEnum)) xs
    where s = fromIntegral . fromEnum . sum $ fmap fst xs
