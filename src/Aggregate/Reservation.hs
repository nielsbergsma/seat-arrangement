module Aggregate.Reservation
  ( NumberOfPassengers
  , ReservationIdProblem(..)
  , ReservationId(..)
  , Reservation(..)
  , Reservations
  , ReservationsProblem(..)
  , Passenger(..)
  
  -- operations
  , parseReservationId
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
import Extension.Refined (Refined, pattern Refined, RefineException, Predicate(..), FromTo, type (&&), success, refine, unrefine, parseSomeException, throwRefineSomeException)


type NumberOfPassengers = Refined (FromTo 1 256) Int

data ReservationIdProblem 
  = ReservationIdIsEmpty
  deriving stock (Eq, Show)
  deriving anyclass (Exception)


newtype ReservationId = ReservationId (Refined NotEmpty UUID)
  deriving stock (Eq, Ord, Show)


parseReservationId :: UUID -> Either ReservationIdProblem ReservationId
parseReservationId value = 
  case refine value of
    Left _ -> Left ReservationIdIsEmpty
    Right id' -> Right (ReservationId id')


data Reservation = Reservation
  { reservationId :: ReservationId
  , reservationPassengers :: NumberOfPassengers
  }
  deriving stock (Show)

instance Eq Reservation where
  (==) lhs rhs = lhs.reservationId == rhs.reservationId

instance Ord Reservation where 
  compare lhs rhs = compare lhs.reservationId rhs.reservationId


data Passenger = Passenger ReservationId Int
  deriving stock (Eq, Ord, Show)


type Reservations (sc :: Nat) = Refined (NotEmpty && SeatCapacity sc) (Set Reservation)

-- | Create passengers from reservation
passengersOfReservation :: Reservation -> Set Passenger
passengersOfReservation (Reservation id' (Refined passengers)) = 
  Set.fromList [Passenger id' p | p <- [0..passengers-1]]


-- invariants
data ReservationsProblem
  = ReservationsIsEmpty
  | ReservationsExceedSeatCapacity
  | ReservationsCorrupt Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Parse 'RefineException' to 'ReservationsProblem', falls back to 'ReservationsCorrupt'
parseReservationsProblem :: RefineException -> ReservationsProblem
parseReservationsProblem exception = 
  fromMaybe 
    (ReservationsCorrupt (pack (show exception)))
    (parseSomeException exception >>= fromException)


instance Predicate NotEmpty (Set Reservation) where
  validate p reservations
    | not (null reservations) = success
    | otherwise = throwRefineSomeException (typeRep p) (toException ReservationsIsEmpty)


instance KnownNat sc => Predicate (SeatCapacity sc) (Set Reservation) where
  validate p reservations
    | passengerCount > 0 && passengerCount <= seatCapacity = success
    | otherwise = throwRefineSomeException (typeRep p) (toException ReservationsExceedSeatCapacity)
    where 
      passengerCount = foldr ((+) . unrefine . reservationPassengers) 0 reservations
      seatCapacity = fromInteger (natVal (Proxy @sc))