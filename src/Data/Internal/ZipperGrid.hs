module Data.Internal.ZipperGrid where

import Data.Internal.Nat
import Data.Maybe        (fromJust)

data Direction = U | D | L | R deriving (Bounded, Enum, Eq, Show)

data Zipper a = Z [a] a Nat [a]

instance Show a => Show (Zipper a) where
  show (Z ls v _ rs) = show (reverse ls ++ v : rs)

instance Functor Zipper where
  fmap f (Z ls v p rs) = Z (f <$> ls) (f v) p (f <$> rs)

left :: Zipper a -> Maybe (Zipper a)
left (Z (l:ls) v p rs) = Just (Z ls l (pred p) (v:rs))
left (Z []     _ _ _ ) = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Z ls v p (r:rs)) = Just (Z (v:ls) r (succ p) rs)
right (Z _  _ _ []    ) = Nothing

withFocus :: (a -> a) -> Zipper a -> Zipper a
withFocus f (Z ls v p rs) = Z ls (f v) p rs

setFocus :: a -> Zipper a -> Zipper a
setFocus = withFocus . const

getFocus :: Zipper a -> a
getFocus (Z _ a _ _) = a

newtype ZipperGrid a = ZG (Zipper (Zipper a))

position :: ZipperGrid a -> (Nat, Nat)
position (ZG (Z _ (Z _ _ y _) x _)) = (x, y)

get :: ZipperGrid a -> a
get (ZG (Z _ f _ _)) = getFocus f

applyFocus :: (a -> a) -> ZipperGrid a -> ZipperGrid a
applyFocus f (ZG (Z ls a p rs)) = ZG (Z ls (withFocus f a) p rs)

set :: a -> ZipperGrid a -> ZipperGrid a
set = applyFocus . const

move :: Direction -> ZipperGrid a -> Maybe (ZipperGrid a)
move U (ZG z) = ZG <$> left z
move D (ZG z) = ZG <$> right z
move L zg@(ZG z) = case position zg of
  (_, 0) -> Nothing
  _      -> Just . ZG $ fmap (fromJust . left) z
move R (ZG z) = Just . ZG $ fmap (fromJust . right) z
