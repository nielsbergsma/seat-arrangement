module Extension.Data 
  ( headLeft
  , headF
  , sequenceSet
  , maximumTotal
  ) where

import Data.Set (Set)
import Data.Set qualified as Set  
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable


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
  | null items = Nothing
  | otherwise = Just (maximum items)