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
