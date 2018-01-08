module Data.Internal.Nat (Nat()) where

newtype Nat = Nat Int deriving (Eq, Ord)

instance Num Nat where
  fromInteger = Nat . fromInteger
  Nat i + Nat j = Nat (i + j)
  Nat i - Nat j = max minBound (Nat $ i - j)
  Nat i * Nat j = Nat (i * j)
  abs n = n
  signum (Nat n) = Nat (signum n)

instance Show Nat where show (Nat n) = show n
instance Read Nat where
  readsPrec n = fmap (first fromInteger) . readsPrec n
    where first f (a, b) = (f a, b)

instance Enum Nat where
  toEnum = Nat
  fromEnum (Nat n) = n
  succ n = n + 1
  pred n = n - 1

instance Bounded Nat where
  minBound = 0
  maxBound = Nat maxBound
