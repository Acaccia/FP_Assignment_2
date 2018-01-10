{-# LANGUAGE Strict #-}
module Data.Worm where

import Control.Concurrent.STM
import Data.Internal.Direction
import Data.Internal.Nat
import System.Random

data Worm = Worm {
    corpse    :: [(Nat, Nat)]
  , direction :: Direction
  , growing   :: Bool
  }

type TWorm = TVar Worm
type TWorms = [TWorm]

initWorm :: (Nat, Nat) -> StdGen -> (Worm, StdGen)
initWorm p g = let (d, g') = random g in (Worm [p] d True, g')

grow, dug :: Worm -> Worm
grow (Worm cor@(c:_) d g) = Worm (move U c : cor) d g
dug (Worm cs d g) = Worm (init cs) d g

living :: Worm -> Bool
living = not . null . corpse

keepGrowing :: Int -> Worm -> Worm
keepGrowing maxSize (Worm cs d g) = Worm cs d (g && length cs < maxSize)

growSTM, dugSTM :: TWorm -> STM ()
growSTM = flip modifyTVar' grow
dugSTM = flip modifyTVar' dug

corpsePositionsSTM :: TWorms -> STM [(Nat, Nat)]
corpsePositionsSTM = fmap concat . traverse (fmap corpse . readTVar)
