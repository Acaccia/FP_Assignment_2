{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
module Data.Worm where

import Control.Concurrent.STM
import Data.Desert
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
grow (Worm cor@(c:_) d g) = Worm (move d c : cor) d g
dug (Worm cs d g) = Worm (init cs) d g

living :: Worm -> Bool
living = not . null . corpse

keepGrowing :: Int -> Worm -> Worm
keepGrowing maxSize (Worm cs d g) = Worm cs d (g && length cs < maxSize)

corpsePositionsSTM :: TWorms -> STM [(Nat, Nat)]
corpsePositionsSTM = fmap concat . traverse (fmap corpse . readTVar)

wormActionSTM :: TWorm -> TWorms -> Desert -> STM ()
wormActionSTM tw tws d =
  readTVar tw >>= \case
    w@(Worm _ _ False)       -> writeTVar tw (dug w)
    w@(Worm cor@(c:_) dir _) -> do
      let newPos = move dir c
      let tile = getTile d newPos
      occupied <- corpsePositionsSTM tws
      writeTVar tw $ if tile /= Sand False && newPos `notElem` occupied
        then Worm (newPos : cor) dir True
        else dug w
