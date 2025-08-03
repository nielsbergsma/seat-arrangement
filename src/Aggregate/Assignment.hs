module Aggregate.Assignment
  ( Assignments
  , ConfirmableAssignments
  , AssignmentsProblem

  -- operations
  , parseAssignmentsProblem
  , partitionConfirmableAssignments
  ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Control.Exception.Base (Exception, fromException, toException)
import Data.Typeable (Proxy(..), typeRep)
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap

import Constraint (BoundedSeatNumber, SeatCapacity)
import Extension.Refined (Refined, pattern Refined, Predicate(..), RefineException, type (&&), parseSomeException, throwRefineSomeException, success, unrefine)

import Aggregate.Reservation (Passenger)
import Aggregate.Seat (Seat, AssignableSeat(..))


type Assignments (sc :: Nat) = Refined (BoundedSeatNumber sc && SeatCapacity sc) (Bimap Passenger Seat)


type ConfirmableAssignments (sc :: Nat) = Refined (BoundedSeatNumber sc && SeatCapacity sc) (Bimap Passenger AssignableSeat)


-- operations
-- | Partitions 'ConfirmableAssignments' into assigned / unassigned
partitionConfirmableAssignments :: ConfirmableAssignments sc -> (Bimap Passenger Seat, Bimap Passenger AssignableSeat)
partitionConfirmableAssignments (Refined assignments) = 
  Bimap.fold partition (Bimap.empty, Bimap.empty) assignments
  where
    partition passenger (AssignedSeat seat) (assigned, unassigned) = (Bimap.insert passenger seat assigned, unassigned)
    partition passenger UnassignedSeat (assigned, unassigned) = (assigned, Bimap.insert passenger UnassignedSeat unassigned)


-- invariants
data AssignmentsProblem 
  = AssignmentsIsEmpty
  | AssignmentsExceedSeatCapacity
  | AssignmentsContainDuplicateSeats
  | AssignmentsContainOutOfRangeSeat
  | AssignmentsCorrupt Text
  deriving stock (Show)
  deriving anyclass (Exception)


-- | Parse 'RefineException' to 'AssignmentsProblem', falls back to 'AssignmentsCorrupt'
parseAssignmentsProblem :: RefineException -> AssignmentsProblem
parseAssignmentsProblem exception = 
  fromMaybe 
    (AssignmentsCorrupt (pack (show exception)))
    (parseSomeException exception >>= fromException)


instance KnownNat sc => Predicate (SeatCapacity sc) (Bimap Passenger s) where
  validate p assignments 
    | seatCount > 0 && seatCount <= seatCapacity = success
    | seatCount == 0 = throwRefineSomeException (typeRep p) (toException AssignmentsIsEmpty)
    | otherwise = throwRefineSomeException (typeRep p) (toException AssignmentsExceedSeatCapacity)
    where 
      seatCount = Bimap.size assignments
      seatCapacity = fromInteger (natVal (Proxy @sc))


instance KnownNat sc => Predicate (BoundedSeatNumber sc) (Bimap Passenger Seat) where
  validate p assignments 
    | maxSeatNumber < seatCapacity = success
    | otherwise = throwRefineSomeException (typeRep p) (toException AssignmentsContainOutOfRangeSeat)
    where 
      maxSeatNumber = maximum (unrefine <$> Bimap.elems assignments)
      seatCapacity = fromInteger (natVal (Proxy @sc))


instance KnownNat sc => Predicate (BoundedSeatNumber sc) (Bimap Passenger AssignableSeat) where
  validate p assignments 
    | seatNumberMax < seatCapacity = success
    | otherwise = throwRefineSomeException (typeRep p) (toException AssignmentsContainOutOfRangeSeat)
    where 
      seatNumberMax = maximum (assignableSeatToInt <$> Bimap.elems assignments)
      seatCapacity = fromInteger (natVal (Proxy @sc))

      assignableSeatToInt UnassignedSeat = 0
      assignableSeatToInt (AssignedSeat (Refined seatNumber)) = seatNumber
