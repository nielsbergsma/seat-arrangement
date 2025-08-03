module Constraint
  ( NotEmpty
  , SeatCapacity
  , BoundedSeatNumber
  , UuidProblem(..)
  ) where

import GHC.TypeLits (Nat)
import Control.Exception.Base (Exception, toException)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Typeable (typeRep)
import Extension.Refined (Predicate(..), success, throwRefineSomeException)


-- | A non-empty constraint, for containers it means 1+ inhabitants, for uuid it means that it's not zeroed, etc.
data NotEmpty


-- | 'sc' defines the upper bound capacity, aka the number of seats, range [1 - sc]
data SeatCapacity (sc :: Nat)


-- | 'sc' upper bound (exclusive) seat number, range [0 - sc)
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