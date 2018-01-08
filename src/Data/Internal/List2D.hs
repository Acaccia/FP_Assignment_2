module Data.Internal.List2D (List2D(..), (!), set, Nat, Index) where

import Data.Internal.Nat

newtype List2D a = List2D [[a]]

type Index = (Nat, Nat)

instance Functor List2D where
  fmap f (List2D l) = List2D $ fmap f <$> l

instance Show a => Show (List2D a) where
  show (List2D a) = show a

(!) :: List2D a -> Index -> a
List2D xs ! (i, j) = xs !! fromEnum i !! fromEnum j

set :: a -> Index -> List2D a -> List2D a
set a (i, j) (List2D l2d) =
  let (xs, y:ys) = splitAt (fromEnum i) l2d
      (xxs, _:yys) = splitAt (fromEnum j) y
  in List2D $ xs ++ [xxs ++ [a] ++ yys] ++ ys

test :: List2D (Nat, Nat)
test = List2D [[(x, y) | y <- [0..]] | x <- [0..]]
