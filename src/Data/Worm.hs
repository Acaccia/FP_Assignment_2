{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
module Data.Worm (Worm(..), TWorms(..), addWorm, corpsePositionsSTM, initTWorms, wormsMove, saveWormsSTM) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Desert
import Data.Internal.Direction
import Data.Internal.Nat
import Data.List                (intercalate)
import System.Random
import Text.Printf

data Worm = Worm {
    corpse  :: [(Nat, Nat)]
  , growing :: Bool
  }

type TWorm = TVar Worm
data TWorms = TWorms {
    worms :: [TWorm]
  , g     :: TMVar StdGen
  }

initWorm :: (Nat, Nat) -> Worm
initWorm p = Worm [p] True

initTWorms :: IO TWorms
initTWorms = TWorms [] <$> (newStdGen >>= newTMVarIO)

dug :: Worm -> Worm
dug (Worm cs _) = Worm (init cs) False

living :: Worm -> Bool
living = not . null . corpse

keepGrowing :: Int -> Worm -> Worm
keepGrowing maxSize (Worm cs g) = Worm cs (g && length cs < maxSize)

corpsePositionsSTM :: TWorms -> STM [(Nat, Nat)]
corpsePositionsSTM = fmap concat . traverse (fmap corpse . readTVar) . worms

getDirection :: TWorms -> STM [Direction]
getDirection (TWorms _ gen) = do
  g <- takeTMVar gen
  let (d, g') = randomDirections g
  putTMVar gen g'
  pure d

wormActionSTM :: Desert -> TWorms -> TWorm -> STM ()
wormActionSTM d tws tw =
  readTVar tw >>= \case
    w@(Worm _ False)     -> writeTVar tw (dug w)
    w@(Worm cor@(c:_) _) -> do
      dir <- getDirection tws
      occupied <- corpsePositionsSTM tws
      let newPos = filter (`elem` occupied) $ flip move c <$> dir
      let tiles = getTile d <$> newPos
      process newPos tiles
      where process (p:ps) (t:ts) = if t /= Sand False
                  then writeTVar tw (Worm (p : cor) True)
                  else process ps ts
            process [] [] = writeTVar tw (dug w)

wormAction :: Desert -> TWorms -> TWorm -> IO ()
wormAction d tws tw = atomically (wormActionSTM d tws tw)

wormsTurn :: Desert -> TWorms -> IO ()
wormsTurn d = forConcurrently_ <$> worms <*> wormAction d

livingSTM :: TWorm -> STM Bool
livingSTM = fmap living . readTVar

removeOldWormsSTM :: TWorms -> STM TWorms
removeOldWormsSTM (TWorms w g) = flip TWorms g <$> filterM livingSTM w

wormsMove :: Desert -> TWorms -> IO TWorms
wormsMove d tw = wormsTurn d tw >> atomically (removeOldWormsSTM tw)

addWorm :: Double -> (Nat, Nat) -> TWorms -> IO TWorms
addWorm p pos tws = do
  gen <- atomically $ takeTMVar (g tws)
  let (r, gen') = random gen
  atomically $ putTMVar (g tws) gen'
  (if r < p then put (newTVarIO (initWorm pos)) else pure) tws
  where put w t = (\w' -> t {worms = w' : worms t}) <$> w


saveWormsSTM :: TWorms -> STM String
saveWormsSTM = fmap (intercalate "\n") . traverse saveWorm . worms
  where positions = intercalate "," . fmap (\(x, y) -> printf "[%d,%d]" (fromEnum x) (fromEnum y))
        toString (Worm cs True)  = printf "emerging (%s)" (positions cs)
        toString (Worm cs False) = printf "disappearing (%s)" (positions cs)
        saveWorm = fmap toString . readTVar
