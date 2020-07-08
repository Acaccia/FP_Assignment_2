{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Persistency (saveGame, loadGame) where

import Control.Concurrent.STM
import Control.Lens
import Data.Config
import Data.Desert            hiding (collected, revealed)
import Data.HashSet           (fromList)
import Data.Internal.Nat
import Data.Player            (Player (..), savePlayer)
import Data.Worm              hiding (g)
import System.Random
import Text.Parsec

saveGame :: FilePath -> Config -> Desert -> Player -> TWorms -> IO ()
saveGame path c d p tws = do
  writeFile path (savePlayer p)
  appendFile path (saveDesert d)
  atomically (saveWormsSTM tws) >>= appendFile path
  appendFile path (saveConfig c)

type Position = (Nat, Nat)

data GameParse = GameParse {
    _position     :: Maybe Position
  , _supply       :: Maybe Int
  , _revealed     :: [Position]
  , _collected    :: [Position]
  , _emerging     :: [[Position]]
  , _disappearing :: [[Position]]
  , _s            :: Maybe Int
  , _m            :: Maybe Int
  , _g            :: Maybe Int
  , _t            :: Maybe Nat
  , _w            :: Maybe Nat
  , _p            :: Maybe Nat
  , _l            :: Maybe Nat
  , _ll           :: Maybe Nat
  , _x            :: Maybe Int
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
parseGameParse = many (allparsers *> newline) *> eof *> getState
  where allparsers = try parsePos <|> try parseSup <|> try parseRev <|> try parseCol <|> try parseEmer <|> try parseDis <|> try parseS <|> try parseM <|> try parseG <|> try parseT <|> try parseW <|> try parseP <|> try parseL <|> try parseLL <|> try parseX <|> parseY

loadGame :: FilePath -> IO (Maybe (Config, Desert, Player, TWorms))
loadGame path = runParser parseGameParse initGameParse "Load Game" <$> readFile path >>= \case
  Left err -> putStrLn "Corrupted save" >> pure Nothing
  Right gp -> case (configLoad gp, desertLoad gp, playerLoad gp) of
    (Just c, Just d, Just p) ->
      (\w -> Just (c, d, p, w)) <$> wormLoad gp
    _err -> putStrLn "Corrupted save" >> pure Nothing

configLoad :: GameParse -> Maybe Config
configLoad gp = Config <$> view s gp <*> view m gp <*> view g gp
            <*> viewD t gp <*> viewD w gp <*> viewD p gp
            <*> viewD l gp <*> viewD ll gp <*> view x gp
            <*> viewD y gp
  where viewD l = fmap ((/ 100) . fromIntegral) . view l

desertLoad :: GameParse -> Maybe Desert
desertLoad gp = do
    (t', w', p', l', ll', g') <- (,,,,,) <$> viewD t gp
        <*> viewD w gp <*> viewD p gp <*> viewD l gp
        <*> viewD ll gp <*> view g gp
    let l = list2D $ makeDesert t' w' p' l' ll' 0 (mkStdGen g')
    let c = fromList $ view collected gp
    let r = fromList $ view revealed gp
    pure (Desert l r c)
  where viewD l = fmap ((/ 100) . fromIntegral) . view l

playerLoad :: GameParse -> Maybe Player
playerLoad gp = Player <$> view position gp <*> view supply gp

wormLoad :: GameParse -> IO TWorms
wormLoad gp = do
  g <- newStdGen >>= newTMVarIO
  em <- traverse (newTVarIO . flip Worm True) (view emerging gp)
  di <- traverse (newTVarIO . flip Worm False) (view disappearing gp)
  pure $ TWorms (em ++ di) g
