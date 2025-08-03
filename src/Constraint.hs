module Constraint
  ( NotEmpty
  , SeatCapacity
  , BoundedSeatNumber
  ) where

import GHC.TypeLits (Nat)
import Control.Exception.Base (Exception, toException)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Typeable (typeRep)
import Extension.Refined (Predicate(..), success, throwRefineSomeException)


data NotEmpty

data SeatCapacity (sc :: Nat)

data BoundedSeatNumber (sc :: Nat)


-- | Generic implementations
data UuidProblem
  = UuidIsEmpty
  deriving stock (Show)
  deriving anyclass (Exception)

instance Predicate NotEmpty UUID where
  validate p uuid 
    | not (uuid == UUID.nil) = success
    | otherwise = throwRefineSomeException (typeRep p) (toException UuidIsEmpty)