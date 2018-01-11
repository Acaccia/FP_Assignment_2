module Data.Player (Player(..), D.Direction(..), move, refillWater, savePlayer) where

import qualified Data.Internal.Direction as D
import           Data.Internal.Nat
import           Text.Printf

data Player = Player {
    pos   :: (Nat, Nat)
  , water :: Int
  } deriving Show

move :: D.Direction -> Player -> Player
move d p@(Player pos _) = let newPos = D.move d pos in
  if newPos == pos then p else p {pos = newPos, water = water p - 1}

refillWater :: Int -> Player -> Player
refillWater n p = p {water = n}

savePlayer :: Player -> String
savePlayer (Player (x, y) w) = printf "position ([%d,%d])\nsupply (%d)" (fromEnum x) (fromEnum y) w
