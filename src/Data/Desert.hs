{-# LANGUAGE MultiWayIf #-}
module Data.Desert (Tile(..), Desert, makeDesert, observe, (!), set, Index, openChest) where

import Control.Monad.State
import Data.Internal.List2D
import System.Random
import Text.Printf          (printf)

data Tile = Sand Bool | Water | Lava | Portal deriving (Eq)

instance Show Tile where
  show = pure . toChar

toChar :: Tile -> Char
toChar (Sand _) = '.'
toChar Water    = '_'
toChar Lava     = '~'
toChar Portal   = '!'

openChest :: (Nat, Nat) -> Desert -> Desert
openChest = set (Sand False)

type Desert = List2D Tile

makeDesert :: Double -> Double -> Double -> Double -> Double -> StdGen -> Desert
makeDesert t w p l ll g = List2D (headLine : tailLines) where

    (s:seeds) = mkStdGen <$> randoms g

    randomTile :: State (StdGen, Tile, [Tile]) Tile
    randomTile = do
      (g, tileLeft, tileUp : ts) <- get
      let (r, g') = random g
      let l' = if Lava `elem` [tileLeft, tileUp] then ll else l
      let p' = p + w
      let (tile, gg) = if | r < w      -> (Water, g')
                          | r < p'     -> (Portal, g')
                          | r < p' + l -> (Lava, g')
                          | otherwise  -> let (r, g'') = random g' in (Sand (r < t), g'')
      put (gg, tile, ts)
      pure tile

    lineOfTiles :: State (StdGen, Tile, [Tile]) [Tile]
    lineOfTiles = (:) <$> randomTile <*> lineOfTiles

    headLine :: [Tile]
    headLine = Sand False : evalState lineOfTiles (s, Sand False, repeat (Sand False))

    tailLines :: [[Tile]]
    tailLines = evalState lineOfTiles <$> zip3 seeds (repeat $ Sand False) (headLine : tailLines)

observe :: (Nat, Nat) -> Int -> Desert -> String
observe (x, y) sight (List2D grid) = unlines $ putPlayer formatted
  where x' = fromEnum x
        y' = fromEnum y
        goodLines = dropAndTake (x'-sight) (2*sight + 1) grid
        goodCols = zipWith takeAround ([0..sight] ++ [sight-1, sight-2..]) goodLines
        formatted = zipWith format ([sight+1, sight+2..sight*2] ++ [sight*2+1, sight*2..]) goodCols

        dropAndTake :: Int -> Int -> [[a]] -> [[a]]
        dropAndTake d t xs = if d < 0
          then replicate (abs d) [] ++ take (t+d) xs
          else take t (drop d xs)

        takeAround :: Int -> [a] -> [a]
        takeAround n = drop (y'-n) . take (y'+n+1)

        format :: Int -> [Tile] -> String
        format n = printf ("% " ++ show n ++ "s") . fmap toChar

        putPlayer :: [String] -> [String]
        putPlayer grid =
          let (xs, y:ys) = splitAt sight grid
              (xxs, _:yys) = splitAt sight y
          in xs ++ [xxs ++ "P" ++ yys] ++ ys

testDesert :: Desert
testDesert = makeDesert 0.3 0.1 0.05 0.1 0.5 (mkStdGen 42)
