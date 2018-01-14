{-# LANGUAGE TemplateHaskell #-}
module Graphics.GlossyDesert where

import Control.Lens
import Data.Desert
import Data.Internal.Nat
import Data.Player
import Data.Worm
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

type SaveFile = FilePath

data GameState = GameState {
    _desert :: Desert
  , _player :: Player
  , _worms  :: TWorms
  }

makeLenses ''GameState

testDesert :: Desert
testDesert = makeDesert 0.3 0.1 0.05 0.1 0.5 10 (mkStdGen 42)

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

window = InWindow "Desert" (1000, 1000) (0, 0)

background = black

drawing (i, j) = pictures [drawingDesert (i, j) testDesert, drawingPlayer (i, j), drawingWorms (i, j) [(1, 1), (4, 5), (5000, 12)]]
drawing' = color white $ scale 0.35 0.35 $ text "1"

main :: IO ()
main = display window background $ drawing (0, 0)
