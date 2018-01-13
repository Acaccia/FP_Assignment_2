module Data.Internal.Direction where

import Data.Bifunctor
import Data.Internal.Nat
import System.Random

data Direction = U | D | L | R deriving (Bounded, Enum, Eq, Show)

instance Random Direction where
  randomR (a, b) = first toEnum . randomR (fromEnum a, fromEnum b)
  random = randomR (minBound, maxBound)

move :: Enum a => Direction -> (a, a) -> (a, a)
move U = first pred
move D = first succ
move L = second pred
move R = second succ

opposed :: Direction -> Direction
opposed U = D
opposed D = U
opposed L = R
opposed R = L

randomDirections :: StdGen -> ([Direction], StdGen)
randomDirections g =
  let ((a, as), g') = first (`pick` [U, D, L, R]) (randomR (0, 3) g)
      ((b, bs), g'') = first (`pick` as) (randomR (0, 2) g')
      ((c, [d]), g''') = first (`pick` bs) (randomR (0, 1) g'')
  in ([a, b, c, d], g''')
  where pick :: Int -> [Direction] -> (Direction, [Direction])
        pick 0 (x:xs) = (x, xs)
        pick n (x:xs) = (x :) <$> pick (n-1) xs
