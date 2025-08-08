module Aggregate.Assignment
  ( Assignments
  , ConfirmableAssignments
  , AssignmentsProblem(..)

  -- operations
  , parseAssignmentsProblem
  , partitionConfirmableAssignments
  ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Control.Exception.Base (Exception, fromException, toException)
import Data.Typeable (Proxy(..), typeRep)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Extension.Data (maximumTotal)

import Constraint (BoundedSeatNumber, SeatCapacity)
import Extension.Refined (Refined, pattern Refined, Predicate(..), RefineException, type (&&), parseSomeException, throwRefineSomeException, success, unrefine)

import Aggregate.Reservation (Passenger)
import Aggregate.Seat (Seat, AssignableSeat(..))


type Assignments (sc :: Nat) = Refined (SeatCapacity sc && BoundedSeatNumber sc) (Bimap Passenger Seat)


type ConfirmableAssignments (sc :: Nat) = Refined (SeatCapacity sc && BoundedSeatNumber sc) (Bimap Passenger AssignableSeat)


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
  | AssignmentsContainOutOfRangeSeat
  | AssignmentsCorrupt Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)


-- | Parse 'RefineException' to 'AssignmentsProblem', falls back to 'AssignmentsCorrupt'
parseAssignmentsProblem :: RefineException -> AssignmentsProblem
parseAssignmentsProblem exception = 
  fromMaybe 
    (AssignmentsCorrupt (Text.show exception))
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
    | maxSeatNumber < Just seatCapacity = success
    | otherwise = throwRefineSomeException (typeRep p) (toException AssignmentsContainOutOfRangeSeat)
    where 
      maxSeatNumber = maximumTotal (unrefine <$> Bimap.elems assignments)
      seatCapacity = fromInteger (natVal (Proxy @sc))


instance KnownNat sc => Predicate (BoundedSeatNumber sc) (Bimap Passenger AssignableSeat) where
  validate p assignments 
    | seatNumberMax < Just seatCapacity = success
    | otherwise = throwRefineSomeException (typeRep p) (toException AssignmentsContainOutOfRangeSeat)
    where 
      seatNumberMax = maximumTotal (assignableSeatToMax <$> Bimap.elems assignments)
      seatCapacity = fromInteger (natVal (Proxy @sc))

      assignableSeatToMax UnassignedSeat = 0
      assignableSeatToMax (AssignedSeat (Refined seatNumber)) = seatNumber


