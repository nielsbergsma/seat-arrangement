module Extension.Data 
  ( headLeft
  , headF
  , sequenceSet
  , maximumTotal
  ) where

import Data.Set (Set)
import qualified Data.Set as Set  
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable


headLeft :: (Foldable t) => t (Either l r) -> Maybe l
headLeft items = case Foldable.find Either.isLeft items of
  Just (Left left) -> Just left
  _ -> Nothing


headF :: (Foldable t) => t a -> Maybe a
headF items
  | a : _ <- Foldable.toList items = Just a
  | otherwise = Nothing


sequenceSet :: (Foldable t, Ord r) => t (Either l r) -> Either l (Set r)
sequenceSet = 
  fmap Set.fromList . sequence . Foldable.toList


maximumTotal :: (Foldable t, Ord a) => t a -> Maybe a
maximumTotal items
  | Foldable.null items = Nothing
  | otherwise = Just (maximum items)