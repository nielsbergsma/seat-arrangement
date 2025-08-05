module Aggregate.AssignmentSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.TypeLits (someNatVal)
import Control.Exception.Base (fromException)
import Data.Maybe (fromJust)
import Data.Either (isRight, isLeft)
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Extension.Refined (pattern Refined, RefineException, refine, parseSomeException)

import Aggregate.Seat
import Aggregate.Reservation 
import Aggregate.Assignment


tests :: TestTree
tests = testGroup "Assignment tests"
  [ testAssignments
  , testAssignmentsNotEmpty
  , testAssignmentsSeatCapacity
  , testAssignmentsBoundedSeats
  , testConfirmableAssignments
  , testConfirmableAssignmentsNonEmpty
  , testConfirmableAssignmentsSeatCapacity
  , testConfirmableAssignmentsUnassigned
  , testConfirmableAssignmentsBoundedSeats
  , testPartitionConfirmableAssignments
  ]


testAssignments = testCase "Assignments must initialise with assignments" $ do
  expectRight actual
  where
    actual = refine assignments :: Either RefineException (Assignments 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, mkSeat 0)
      ,(mkPassenger "7bac6594-5cd9-477c-a16e-fe88096f6c6b" 0, mkSeat 1)
      ]

testAssignmentsNotEmpty = testCase "Assignments must be non-empty" $ do
  expectLeftProblem AssignmentsIsEmpty actual
  where
    actual = refine Bimap.empty :: Either RefineException (Assignments 2)


testAssignmentsSeatCapacity = testCase "Assignments must be within seat capacity" $ do
  expectLeftProblem AssignmentsExceedSeatCapacity actual
  where
    actual = refine assignments :: Either RefineException (Assignments 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, mkSeat 0)
      ,(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 1, mkSeat 1)
      ,(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 2, mkSeat 2)
      ]


testAssignmentsBoundedSeats = testCase "Assignments seats must be between 0 and sc" $ do
  expectLeftProblem AssignmentsContainOutOfRangeSeat actual
  where
    actual = refine assignments :: Either RefineException (Assignments 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, mkSeat 2)
      ]


testConfirmableAssignments = testCase "ConfirmableAssignments must initialise with assignable seats" $ do
  expectRight actual
  where
    actual = refine assignments :: Either RefineException (ConfirmableAssignments 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, AssignedSeat (mkSeat 0))
      ]


testConfirmableAssignmentsNonEmpty = testCase "ConfirmableAssignments must be non-empty" $ do
  expectLeftProblem AssignmentsIsEmpty actual
  where
    actual = refine Bimap.empty :: Either RefineException (ConfirmableAssignments 2)
    

testConfirmableAssignmentsSeatCapacity = testCase "ConfirmableAssignments must be within seat capacity" $ do
  expectLeftProblem AssignmentsExceedSeatCapacity actual
  where
    actual = refine assignments :: Either RefineException (ConfirmableAssignments 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, UnassignedSeat)
      ,(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 1, UnassignedSeat)
      ,(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 2, UnassignedSeat)
      ]


testConfirmableAssignmentsUnassigned = testCase "ConfirmableAssignments must allow multiple unassigned seats" $ do
  expectRight actual
  where
    actual = refine assignments :: Either RefineException (ConfirmableAssignments 3)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, UnassignedSeat)
      ,(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 1, UnassignedSeat)
      ,(mkPassenger "e514bd86-4b07-4074-91af-8f0b75e15bb7" 0, UnassignedSeat)
      ]


testConfirmableAssignmentsBoundedSeats = testCase "ConfirmableAssignments must only allow seat numbers be between 0 and sc" $ do
  expectLeftProblem AssignmentsContainOutOfRangeSeat actual
  where
    actual = refine assignments :: Either RefineException (ConfirmableAssignments 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, AssignedSeat (mkSeat 2))
      ]


testPartitionConfirmableAssignments = testCase "partitionConfirmableAssignments must partition assignments in assigned/unassigned" $ do
  assigned @?= Bimap.fromList [assignment1', assignment2']
  Bimap.keys unassigned @?= [assignment3', assignment4']
  where
    (assigned, unassigned) = partitionConfirmableAssignments assignments
    Right assignments = refine (Bimap.fromList [assignment1, assignment2, assignment3, assignment4]) :: Either RefineException (ConfirmableAssignments 4)

    assignment1  = (mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, AssignedSeat (mkSeat 1))
    assignment1' = (mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, mkSeat 1)
    assignment2  = (mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 1, AssignedSeat (mkSeat 2))
    assignment2' = (mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 1, mkSeat 2)
    assignment3  = (mkPassenger "54cc195c-df77-4b81-b561-23f8b66872cf" 0, UnassignedSeat)
    assignment3' = (mkPassenger "54cc195c-df77-4b81-b561-23f8b66872cf" 0)
    assignment4  = (mkPassenger "54cc195c-df77-4b81-b561-23f8b66872cf" 1, UnassignedSeat)
    assignment4' = (mkPassenger "54cc195c-df77-4b81-b561-23f8b66872cf" 1)


-- generators
mkPassenger idString index = 
  Passenger (mkReservationId idString) index

mkSeat number = seat 
  where
    Right seat = refine number

mkReservationId idString = reservationId
  where 
    Right reservationId = parseReservationId (fromJust (UUID.fromString idString))

-- helpers
expectRight = assertBool "expect right" . isRight

expectLeft = assertBool "expect left" . isLeft

expectLeftProblem problem = \case
  Left exception -> Just problem @?= (parseSomeException exception >>= fromException)  
  Right _ -> assertFailure "expect left"
