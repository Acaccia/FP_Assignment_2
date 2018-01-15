{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.GameGUI (game) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Config
import           Data.Desert                      hiding (set)
import qualified Data.HashSet                     as S
import           Data.Internal.List2D.BFS.Strict
import           Data.Internal.Nat
import           Data.Persistency
import           Data.Player
import           Data.Worm                        hiding (worms)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.IO
import           System.Random

type SaveFile = FilePath

data Status = Ongoing | Win | Lost deriving (Eq)

data GameState = GameState {
    _desert   :: Desert
  , _player   :: Player
  , _worms    :: TWorms
  , _config   :: Config
  , _nearestW :: Maybe Nat
  , _nearestT :: Maybe Nat
  , _nearestP :: Maybe Nat
  , _saveFile :: FilePath
  , _status   :: Status
  }

makeLenses ''GameState

game :: IO ()
game = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  gs <- askNewOrLoad
  playIO window black 30 gs drawingGame handleEvents (const updateState)

askNewOrLoad :: IO GameState
askNewOrLoad = do
  putStr "(1) New Game or (2) Load Game ?"
  getChar >>= \case
      '1' -> do c <- askConfigUntilUserBecomesClever
                f <- putStr "Path for save: " *> getLine
                newGameState c f
      '2' -> do l <- putStr "Path to load: " *> getLine
                s <- putStr "Path to save: " *> getLine
                loadGameState l s
      _   -> askNewOrLoad

newGameState :: Config -> SaveFile -> IO GameState
newGameState c@(Config sight mw seed t w p l ll x y) path =
  (\wms -> GameState
   (makeDesert t w p l ll sight (mkStdGen seed))
   (Player (0, 0) mw) wms c Nothing Nothing Nothing
   path Ongoing) <$> initTWorms

loadGameState :: FilePath -> SaveFile -> IO GameState
loadGameState load save = flip fmap (loadGame load) $ \case
  Nothing -> error "Corrupted File"
  Just (c, d, p, tw) -> GameState d p tw c Nothing Nothing Nothing save Ongoing

tileToPicture :: Maybe Tile -> Picture
tileToPicture t = case t of
    Nothing       -> rect white
    Just (Sand _) -> rect yellow
    Just Portal   -> rect green
    Just Water    -> rect (light blue)
    Just Lava     -> rect (dark red)
  where rect col = color col (rectangleSolid 40 40)

drawingDesert :: (Nat, Nat) -> Desert -> Picture
drawingDesert p = pictures . zipWith (uncurry translate) [(x, y) | y <- [480,440..(-480)], x <- [(-480), (-440)..480]] . fmap tileToPicture . surroundingsArray p 12 12

drawingPlayer :: (Nat, Nat) -> Picture
drawingPlayer (i, j) = translate x y $ color black $ circleSolid 15
  where x = if i >= 12 then 0 else [(-480), (-440)..] !! fromEnum i
        y = if j >= 12 then 0 else [480, 440..] !! fromEnum j

drawingWorms :: (Nat, Nat) -> [(Nat, Nat)] -> Picture
drawingWorms (i, j) = pictures . fmap (draw . relativeCoord) . filter inRange
  where inRange (x, y) = i - 12 < x && i + 12 > x && j - 12 < y && j + 12 > y
        relativeCoord (x, y) = ([(-480), (-440)..480] !! fromEnum (x - i), [480,440..(-480)] !! fromEnum (y - j))
        draw (x, y) = translate x y $ color (greyN 0.4) $ circleSolid 20

window = InWindow "Desert" (1500, 1000) (0, 0)

drawingBoardOngoing :: GameState -> IO Picture
drawingBoardOngoing gs = do
  let playerPos = views player pos gs
  let dView = views desert (drawingDesert playerPos) gs
  wView <- drawingWorms playerPos <$> views worms (atomically . corpsePositionsSTM) gs
  pure $ pictures [dView, drawingPlayer playerPos, wView]

drawingNearests :: GameState -> Picture
drawingNearests gs = pictures $ (scale 0.2 0.2 . color white) <$> [
    translate 0 150 $ text $ "Nearest Treasure: " ++ views nearestT show gs
  , text $ "Nearest Water: " ++ views nearestW show gs
  , translate 0 (-150) $ text $ "Nearest Portal: " ++ views nearestP show gs
  ]

drawingOngoing :: GameState -> IO Picture
drawingOngoing gs = do
  b <- drawingBoardOngoing gs
  pure $ pictures [translate (-250) 0 b, translate 300 0 $ drawingNearests gs]

drawingWin :: GameState -> IO Picture
drawingWin gs = pure $ color (light blue) $ text $ "YOU WIN " ++ views desert (show.collectedTreasures) gs ++ " TREASURES!!"

drawingLost :: IO Picture
drawingLost = pure $ color (dark red) $ text "YOU LOSE!!!"

drawingGame :: GameState -> IO Picture
drawingGame gs = case view status gs of
  Ongoing -> drawingOngoing gs
  Win     -> drawingWin gs
  Lost    -> drawingLost

moveState :: Direction -> GameState -> IO GameState
moveState dir gs = do
  let gs' = over player (move dir) gs
  let d = view desert gs
  w <- wormsMove (view desert gs') (view worms gs')
  pure $ set worms w gs'

handleEvents :: Event -> GameState -> IO GameState
handleEvents (EventKey (Char 'k') Down _ _) =
  (saveGame <$> view saveFile <*> view config <*> view desert
  <*> view player <*> view worms) *> pure
handleEvents (EventKey (Char 'w') Down _ _) = moveState U
handleEvents (EventKey (Char 'a') Down _ _) = moveState L
handleEvents (EventKey (Char 's') Down _ _) = moveState D
handleEvents (EventKey (Char 'd') Down _ _) = moveState R

newWorms :: GameState -> IO GameState
newWorms gs = do
  wmsP <- views worms (atomically . corpsePositionsSTM) gs
  let prob = views config wormLL gs
  let ind = S.filter (\p -> views desert ((! p) . list2D) gs == Sand False) . S.filter (`notElem` wmsP) $ views desert revealed gs
  undefined
  foldM (reduce prob) gs ind
  where reduce prob g i = flip (set worms) g <$> addWorm prob i (view worms g)

updateState :: GameState -> IO GameState
updateState gs = do
  let p = views player pos gs
  wormsPos <- views worms (atomically . corpsePositionsSTM) gs
  pure (if p `elem` wormsPos
          then set status Lost gs
          else processTile p) >>= nearests p >>= newWorms
  where checkWater = if views player water gs <= 0
                      then const Lost
                      else id
        processTile p = case views desert ((! p) . list2D) gs of
          Sand True -> over desert (openChest p) $ over status checkWater gs
          Water -> over player (refillWater $ views config maxWater gs) gs
          Lava -> set status Lost gs
          Portal -> set status Win gs
          _ -> over status checkWater gs
        nearests pos gs' = do
          (w, t, p) <- runConcurrently $ (,,) <$>
            Concurrently (pure $ bfsDistance Water [Lava, Portal] pos $ views desert list2D gs') <*>
            Concurrently (pure $ bfsDistance (Sand True) [Lava, Portal] pos $ views desert list2D gs') <*>
            Concurrently (pure $ bfsDistance Portal [Lava] pos $ views desert list2D gs')
          pure $ set nearestT t $ set nearestW w $ set nearestP p gs'
