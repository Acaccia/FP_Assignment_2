module Data.Persistency where

import Control.Concurrent.STM
import Data.Config
import Data.Desert
import Data.Player
import Data.Worm

saveGame :: FilePath -> Config -> Desert -> Player -> TWorms -> IO ()
saveGame path c d p tws = do
  writeFile path (savePlayer p)
  appendFile path ('\n' : saveDesert d)
  atomically (saveWormsSTM tws) >>= appendFile path . ('\n' :)
  appendFile path ('\n' : saveConfig c)
