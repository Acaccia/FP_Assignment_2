{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Data.Play where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Parallel.Strategies
import qualified Data.Config                     as Conf
import           Data.Desert
import           Data.Functor
import           Data.Internal.List2D.BFS.Strict
import           Data.Internal.Nat
import           Data.Player
import           System.Random                   (mkStdGen)

data Play = Play { _player :: Player
                 , _desert :: Desert
                 }
makeLenses ''Play

data InGameConfig = IGConfig { _maxWater :: Int
                             , _sight    :: Int
                             }
makeLenses ''InGameConfig

data Situation = Win Int | Lost | Ongoing

instance Show Situation where
  show (Win 1) = "You win: 1 treasure found!"
  show (Win n) = "You win: " ++ show n ++ " treasures found!"
  show Lost    = "You lose!"
  show Ongoing = ""

initPlay :: Conf.Config -> Play
initPlay (Conf.Config sight mw seed t w p l ll x y) =
   Play (Player (0, 0) mw) (makeDesert t w p l ll sight (mkStdGen seed))

inGameConfig :: Conf.Config -> InGameConfig
inGameConfig = IGConfig <$> Conf.maxWater <*> Conf.sight

movePlayer :: MonadState Play m => Char -> m ()
movePlayer 'w' = player %= move U
movePlayer 'a' = player %= move L
movePlayer 's' = player %= move D
movePlayer 'd' = player %= move R
movePlayer _   = pure ()

reactToTile :: (MonadState Play m, MonadReader InGameConfig m) => m Situation
reactToTile = do
  pos <- uses player pos
  (! pos) <$> uses desert list2D >>= \case
    Sand True -> do desert %= openChest pos
                    checkWater
    Water     -> do w <- view maxWater
                    player %= refillWater w
                    pure Ongoing
    Lava      -> pure Lost
    Portal    -> Win <$> uses desert collectedTreasures
    _         -> checkWater

checkWater :: MonadState Play m => m Situation
checkWater = chk <$> uses player water
  where chk 0 = Lost
        chk _ = Ongoing

getClosest :: (Nat, Nat) -> Desert -> (Maybe Nat, Maybe Nat, Maybe Nat)
getClosest pos (Desert d _ c) = runEval $ (,,)
  <$> rpar (bfsDistance Water [Lava, Portal] pos d)
  <*> rpar (bfsDistance (Sand True) [Lava, Portal] pos d)
  <*> rpar (bfsDistance Portal [Lava] pos d)

printGame :: (MonadIO m, MonadReader InGameConfig m, MonadState Play m) => m ()
printGame = do
    s <- view sight
    Player pos w <- use player
    d <- use desert
    let (closestW, closestT, closestP) = getClosest pos d
    liftIO (putStr $ surroundings pos 25 40 d)
    liftIO (putStrLn $ "Collected treasures: " ++ show (collectedTreasures d))
    liftIO (putStrLn $ "Remaining water: " ++ show w)
    liftIO (putStrLn $ "Closest water: " ++ show closestW)
    liftIO (putStrLn $ "Closest treasure: " ++ show closestT)
    liftIO (putStrLn $ "Closest portal: " ++ show closestP)

gameLoop :: (MonadIO m, MonadReader InGameConfig m, MonadState Play m) => m ()
gameLoop = do
  printGame
  liftIO getChar >>= movePlayer
  reactToTile >>= \case
    Ongoing -> gameLoop
    other -> liftIO (print other)
