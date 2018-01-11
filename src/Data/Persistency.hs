{-# LANGUAGE TemplateHaskell #-}
module Data.Persistency where

import Control.Concurrent.STM
import Control.Lens
import Data.Config
import Data.Desert            (Desert, saveDesert)
import Data.Internal.Nat
import Data.Player            (Player, savePlayer)
import Data.Worm
import System.Random
import Text.Parsec

saveGame :: FilePath -> Config -> Desert -> Player -> TWorms -> IO ()
saveGame path c d p tws = do
  writeFile path (savePlayer p)
  appendFile path ('\n' : saveDesert d)
  atomically (saveWormsSTM tws) >>= appendFile path . ('\n' :)
  appendFile path ('\n' : saveConfig c)

type Position = (Nat, Nat)

data GameParse = GameParse {
    _position     :: Maybe Position
  , _supply       :: Maybe Nat
  , _revealed     :: [Position]
  , _collected    :: [Position]
  , _emerging     :: [[Position]]
  , _disappearing :: [[Position]]
  , _s            :: Maybe Nat
  , _m            :: Maybe Nat
  , _g            :: Maybe Nat
  , _t            :: Maybe Nat
  , _w            :: Maybe Nat
  , _p            :: Maybe Nat
  , _l            :: Maybe Nat
  , _ll           :: Maybe Nat
  , _x            :: Maybe Nat
  , _y            :: Maybe Nat
  } deriving Show

makeLenses ''GameParse

initGameParse = GameParse Nothing Nothing [] [] [] [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

type Parser = Parsec String GameParse

parseNat :: (Integral nat, Read nat) =>  Parser nat
parseNat = read <$> many1 digit

parsePosition :: Parser (Nat, Nat)
parsePosition = between (char '[') (char ']') $
  (,) <$> parseNat <*> (char ',' *> parseNat)

parsePositions :: Parser [(Nat, Nat)]
parsePositions = parsePosition `sepBy` char ','

parent :: Parser a -> Parser a
parent = between (char '(') (char ')')

parseGenericAssign :: String -> Parser a -> ASetter GameParse GameParse b (Maybe a) -> Parser ()
parseGenericAssign str p lens = do
  string (str ++ " ")
  res <- parent p
  modifyState (lens ?~ res)

parseGenericCons :: String -> Parser a -> ASetter' GameParse [a] -> Parser ()
parseGenericCons str p lens = do
  string (str ++ " ")
  res <- parent p
  modifyState (over lens (res :))

parsePos, parseSup, parseRev, parseCol, parseEmer, parseDis, parseS, parseM, parseG, parseT, parseW, parseP, parseL, parseLL, parseX, parseY :: Parser ()
parsePos = parseGenericAssign "position" parsePosition position
parseSup = parseGenericAssign "supply" parseNat supply
parseRev = parseGenericCons "revealed" parsePosition revealed
parseCol = parseGenericCons "collected" parsePosition collected
parseEmer = parseGenericCons "emerging" parsePositions emerging
parseDis = parseGenericCons "disappearing" parsePositions disappearing
parseS = parseGenericAssign "s" parseNat s
parseM = parseGenericAssign "m" parseNat m
parseG = parseGenericAssign "g" parseNat g
parseT = parseGenericAssign "t" parseNat t
parseW = parseGenericAssign "w" parseNat w
parseP = parseGenericAssign "p" parseNat p
parseL = parseGenericAssign "l" parseNat l
parseLL = parseGenericAssign "ll" parseNat ll
parseX = parseGenericAssign "x" parseNat x
parseY = parseGenericAssign "y" parseNat y

parseGameParse :: Parser GameParse
parseGameParse = many (allparsers *> newline) *> getState
  where allparsers = parsePos <|> try parseSup <|> try parseRev <|> try parseCol <|> try parseEmer <|> try parseDis <|> try parseS <|> try parseM <|> try parseG <|> try parseT <|> try parseW <|> try parseP <|> try parseL <|> try parseLL <|> try parseX <|> parseY
