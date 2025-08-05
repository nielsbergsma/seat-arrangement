module Aggregate.Seat 
  ( NumberOfSeats
  , Seat
  , AssignableSeat(..)

  -- operations
  , seatCapacity
  ) where

import GHC.TypeLits (SomeNat, someNatVal)
import Data.Maybe (fromMaybe)

import Extension.Refined (Refined, FromTo, unrefine)


type Seat = Refined (FromTo 0 256) Int


type NumberOfSeats = Refined (FromTo 1 256) Int


data AssignableSeat = UnassignedSeat | AssignedSeat Seat
  deriving stock (Show)

instance Eq AssignableSeat where
  (==) (AssignedSeat lhs) (AssignedSeat rhs) = lhs == rhs
  (==) _ _ = False

instance Ord AssignableSeat where
  compare (AssignedSeat lhs) (AssignedSeat rhs) = compare lhs rhs
  compare _ _ = LT


seatCapacity :: NumberOfSeats -> SomeNat
seatCapacity = 
  fromMaybe (error "invalid Nat value") . someNatVal . fromIntegral . unrefine