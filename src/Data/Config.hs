{-# LANGUAGE LambdaCase #-}
module Data.Config (Config(..), askConfigUntilUserBecomesClever, saveConfig) where

import Control.Monad.Except
import Data.List
import System.Random        (StdGen, mkStdGen)
import Text.Printf

data Config = Config {
    sight      :: Int
  , maxWater   :: Int
  , seed       :: Int
  , treasureLL :: Double
  , waterLL    :: Double
  , portalLL   :: Double
  , lava1LL    :: Double
  , lava2LL    :: Double
  , wormSize   :: Int
  , wormLL     :: Double
  } deriving (Show)

data ConfigError = OutOfBounds String String String
                 | PercentageOver100
                 | NotANumber String

instance Show ConfigError where
  show (OutOfBounds q l u) = q ++ ": value is out of bounds [" ++ l ++ " - " ++ u ++ "]"
  show PercentageOver100   = "Sum of all percentages is above 100%"
  show (NotANumber q)      = q ++ ": input is not an integer above 0"

type ECIO = ExceptT ConfigError IO

eitherReadNum :: (Read a, Integral a) => String -> String -> ECIO a
eitherReadNum q s = case reads s of
  [(x, "")] -> if x > 0 then pure x else throwError (NotANumber q)
  _         -> throwError (NotANumber q)

boundCheck :: (Ord a, Show a) => String -> a -> a -> a -> ECIO a
boundCheck q low up n =
  if n > low && n < up then pure n
  else throwError $ OutOfBounds q (show low) (show up)

checkPercentage :: Double -> Double -> Double -> ECIO Double
checkPercentage x y z =
  if x + y + z <= 1 then pure z else throwError PercentageOver100

toPercent :: Int -> Double
toPercent = (/ 100) . fromIntegral

ask :: (Integral a, Read a) => String -> ECIO a
ask question = liftIO (putStr (question ++ ": ") >> getLine)
  >>= eitherReadNum question

askAndCheck :: (Integral a, Ord a, Read a, Show a) => String -> a -> a -> ECIO a
askAndCheck q l u = ask q >>= boundCheck q l u

askConfig :: ECIO Config
askConfig = do
  sight <- ask "sight"
  maxWater <- ask "max water"
  seed <- ask "seed"
  treasureLL <- toPercent <$> askAndCheck "treasure likelihood" 0 100
  waterLL <- toPercent <$> askAndCheck "water likelihood" 0 100
  portalLL <- toPercent <$> askAndCheck "portal likelihood" 0 100
  lava1LL <- toPercent <$> askAndCheck "lava likelihood" 0 100 >>= checkPercentage waterLL portalLL
  lava2LL <- toPercent <$> askAndCheck "lava (adjacent) likelihood" 0 100 >>= checkPercentage waterLL portalLL
  wormSize <- ask "worms maximal size"
  wormLL <- toPercent <$> askAndCheck "worms appearance likelihood" 0 100
  pure $ Config sight maxWater seed treasureLL waterLL portalLL lava1LL lava2LL wormSize wormLL

askConfigUntilUserBecomesClever :: IO Config
askConfigUntilUserBecomesClever = runExceptT askConfig >>= \case
  Left err     -> print err *> askConfigUntilUserBecomesClever
  Right config -> pure config

saveConfig :: Config -> String
saveConfig c = unlines [
    format "s" $ attr sight
  , format "m" $ attr maxWater
  , format "g" $ attr seed
  , format "t" $ attrN treasureLL
  , format "w" $ attrN waterLL
  , format "p" $ attrN portalLL
  , format "l" $ attrN lava1LL
  , format "ll" $ attrN lava2LL
  , format "x" $ attr wormSize
  , format "y" $ attrN wormLL
  ]
  where attr att = att c
        attrN att = round (100 * att c)
        format = printf "%s (%d)"
