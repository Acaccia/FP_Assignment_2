{-# LANGUAGE TemplateHaskell #-}
module Graphics.GlossyDesert where

import Control.Lens
import Data.Desert
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

drawingDesert :: [Maybe Tile] -> Picture
drawingDesert ts = pictures $ zipWith (uncurry translate) [(x, y) | y <- [480,440..(-480)], x <- [(-480), (-440)..480]] (tileToPicture <$> ts)

window = InWindow "Desert" (1000, 1000) (0, 0)

background = black

drawing = drawingDesert (surroundingsArray (0, 0) 12 12 testDesert)
drawing' = color white $ scale 0.35 0.35 $ text "1"

main :: IO ()
main = display window background drawing
