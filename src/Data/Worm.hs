module Data.Worm where

import qualified Data.Internal.Direction as D
import           Data.Internal.Nat

data Worm = Worm {
    headPos   :: (Nat, Nat)
  , direction :: D.Direction
  , size      :: Nat
  , growing   :: Bool
  }

continueGrow :: Nat -> Worm -> Worm
continueGrow maxS (Worm pos dir s g) = Worm pos dir s (g && s < maxS)
