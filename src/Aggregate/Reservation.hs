module Aggregate.Reservation
  ( NumberOfPassengers
  , ReservationId(..)
  , Reservation(..)
  , Reservations
  , ReservationsProblem(..)
  , Passenger(..)
  
  -- operations
  , passengersOfReservation
  , parseReservationsProblem
  ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Control.Exception.Base (Exception, toException, fromException)
import Data.Text (Text, pack)
import Data.Typeable (Proxy(..), typeRep)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.UUID (UUID)

import Constraint (NotEmpty, SeatCapacity)
import Extension.Refined (Refined, pattern Refined, RefineException, Predicate(..), FromTo, type (&&), success, unrefine, parseSomeException, throwRefineSomeException)


type NumberOfPassengers = Refined (FromTo 1 256) Int


newtype ReservationId = ReservationId (Refined NotEmpty UUID)
  deriving stock (Eq, Ord)


data Reservation = Reservation
  { reservationId :: ReservationId
  , reservationPassengers :: NumberOfPassengers
  }

instance Eq Reservation where
  (==) lhs rhs = lhs.reservationId == rhs.reservationId

instance Ord Reservation where 
  compare lhs rhs = compare lhs.reservationId rhs.reservationId


data Passenger = Passenger ReservationId Int
  deriving stock (Eq, Ord)


type Reservations (sc :: Nat) = Refined (NotEmpty && SeatCapacity sc) (Set Reservation)

-- | Create passengers from reservation
passengersOfReservation :: Reservation -> Set Passenger
passengersOfReservation (Reservation id' (Refined passengers)) = 
  Set.fromList [Passenger id' p | p <- enumFromTo 1 passengers]


-- invariants
data ReservationsProblem
  = ReservationsIsEmpty
  | ReservationsHasNoPassenger
  | ReservationsExceedSeatCapacity
  | ReservationsCorrupt Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Parse 'RefineException' to 'ReservationsProblem', falls back to 'ReservationsCorrupt'
parseReservationsProblem :: RefineException -> ReservationsProblem
parseReservationsProblem exception = 
  fromMaybe 
    (ReservationsCorrupt (pack (show exception)))
    (parseSomeException exception >>= fromException)


instance Predicate NotEmpty (Set Reservation) where
  validate p reservations
    | not (Set.null reservations) = success
    | otherwise = throwRefineSomeException (typeRep p) (toException ReservationsIsEmpty)


instance KnownNat sc => Predicate (SeatCapacity sc) (Set Reservation) where
  validate p reservations
    | passengerCount > 0 && passengerCount <= seatCapacity = success
    | passengerCount == 0 = throwRefineSomeException (typeRep p) (toException ReservationsHasNoPassenger)
    | otherwise = throwRefineSomeException (typeRep p) (toException ReservationsExceedSeatCapacity)
    where 
      passengerCount = foldr ((+) . unrefine . reservationPassengers) 0 reservations
      seatCapacity = fromInteger (natVal (Proxy @sc))