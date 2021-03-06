{-# LANGUAGE MultiWayIf #-}
module Data.Desert (Tile(..), Desert(..), makeDesert, getTile, observe, (!), set, Index, openChest, collectedTreasures, surroundings, surroundingsArray, saveDesert) where

import           Control.Monad.State
import qualified Data.Array                  as A
import qualified Data.HashSet                as S
import           Data.Internal.Direction
import           Data.Internal.List2D
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           System.Random
import           Text.Printf                 (printf)

data Tile = Sand Bool | Water | Lava | Portal deriving (Eq)

instance Show Tile where
  show = pure . toChar

toChar :: Tile -> Char
toChar (Sand _) = '.'
toChar Water    = '_'
toChar Lava     = '~'
toChar Portal   = '!'

openChest :: (Nat, Nat) -> Desert -> Desert
openChest p (Desert l o c) = Desert (set (Sand False) p l) o (S.insert p c)

data Desert = Desert {
    list2D    :: List2D Tile
  , revealed  :: S.HashSet (Nat, Nat)
  , collected :: S.HashSet (Nat, Nat)
  }

makeDesert :: Double -> Double -> Double -> Double -> Double -> Int -> StdGen -> Desert
makeDesert t w p l ll sight g = Desert (List2D (headLine : tailLines)) observable S.empty
 where
    (s:seeds) = mkStdGen <$> randoms g
    observable = let s = toEnum sight in S.fromList [(i, j) | i <- [0..s], j <- [0..s-i]]

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

{-# ANN observe "HLint: ignore Use infix" #-}
observe :: (Nat, Nat) -> Direction -> Int -> Desert -> Desert
observe (i, j) d sight (Desert l h c) = Desert l (case d of
    U -> union observeNW observeNE
    D -> union observeSW observeSE
    L -> union observeNW observeSW
    R -> union observeNE observeSE) c
  where
    i' = fromEnum i
    j' = fromEnum j
    toNat x = toEnum x :: Nat
    upI = [i'-sight, i'-sight+1..i']    -- [1, 2, 3, 4, 5, 6]
    downI = [i'+sight, i'+sight-1..i']  -- [11, 10, 9, 8, 7, 6]
    leftJ = [j', j'-1..]
    rightJ = [j'..]
    filterNat xs = [(toNat x, toNat y) | (x, y) <- xs, x >= 0 && y >= 0]
    obs m n = filterNat $ zip m n
    observeNW = obs upI leftJ
    observeSW = obs downI leftJ
    observeNE = obs upI rightJ
    observeSE = obs downI rightJ
    union a b = foldr S.insert h (a ++ b)

surroundings :: (Nat, Nat) -> Int -> Int -> Desert -> String
surroundings (i, j) wid hei (Desert d h c) = unlines is
  where
    i' = fromEnum i
    j' = fromEnum j
    charify (x, y) = if | x < 0 || y < 0 -> '#'
                        | (x', y') `S.member` h -> toChar (d ! (x', y'))
                        | otherwise -> '?'
      where x' = toEnum x
            y' = toEnum y
    is = [[charify (x, y) | y <- [j'-wid..j'+wid]] | x <- [i'-hei..i'+hei]]

surroundingsArray :: (Nat, Nat) -> Int -> Int -> Desert -> [Maybe Tile]
surroundingsArray (i, j) wid hei (Desert d h _) = tiles
  where fromX = max 0 (fromEnum i - hei)
        toX = fromX + 2 * hei
        fromY = max 0 (fromEnum j - wid)
        toY = fromY + 2 * wid
        tilify (x, y) = if (x, y) `S.member` h then Just (d ! (x, y)) else Nothing
        tiles = [tilify (x, y) | x <- toEnum <$> [fromX..toX], y <- toEnum <$> [fromY..toY]]

getTile :: Desert -> (Nat, Nat) -> Tile
getTile (Desert l _ _) = (l !)

collectedTreasures :: Desert -> Int
collectedTreasures = S.size . collected

saveDesert :: Desert -> String
saveDesert (Desert _ rs cs) = unlines $ rev ++ col
  where format ty (x, y) = printf "%s ([%d,%d])" ty (fromEnum x) (fromEnum y)
        rev = format "revealed" <$> S.toList rs
        col = format "collected" <$> S.toList cs

testDesert :: Desert
testDesert = makeDesert 0.3 0.1 0.05 0.1 0.5 10 (mkStdGen 42)
