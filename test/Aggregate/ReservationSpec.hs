module Aggregate.ReservationSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.TypeLits (someNatVal)
import Control.Exception.Base (fromException)
import Data.Maybe (fromJust)
import Data.Either (isRight, isLeft)
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Extension.Refined (pattern Refined, refine, RefineException, parseSomeException)

import Aggregate.Seat
import Aggregate.Reservation 


tests :: TestTree
tests = testGroup "Reservation tests"
  [ testNumberOfPassengers
  , testReservationId
  , testReservationIdEquality
  , testReservationPassengers
  , testReservationsNotEmpty
  , testReservationsSeatCapacity
  ]


testNumberOfPassengers = testCase "NumberOfPassengers must range between 1 and 256" $ do
  expectRight (parseNumberOfSeats 1)
  expectRight (parseNumberOfSeats 256)
  expectLeft (parseNumberOfSeats 0)
  expectLeft (parseNumberOfSeats 257)
  where 
    parseNumberOfSeats value = refine value :: Either RefineException NumberOfPassengers

testReservationId = testCase "ReservationId must be non-empty" $ do
  expectRight (parseReservationId validId)
  expectLeft (parseReservationId invalidId)
  where 
    Just validId = UUID.fromString "8dbde54e-ed7e-4449-bc38-7da529de5603"
    invalidId = UUID.nil


testReservationIdEquality = testCase "ReservationId must be equal by value" $ do
  expectOrdEqual reservationId1 reservationId1
  where 
    Right reservationId1 = parseReservationId (uuidFromString "8dbde54e-ed7e-4449-bc38-7da529de5603")


testReservationPassengers = testCase "Reservation must be able to export its passengers" $ do
  passengersOfReservation reservation @?= Set.fromList [Passenger reservationId 0, Passenger reservationId 1]
  where 
    reservation = mkReservation "8dbde54e-ed7e-4449-bc38-7da529de5603" 2
    reservationId = mkReservationId "8dbde54e-ed7e-4449-bc38-7da529de5603"


testReservationsNotEmpty = testCase "Reservations must be non-empty" $ do
  expectLeftProblem ReservationsIsEmpty reservations
  where 
    reservations = refine Set.empty :: Either RefineException (Reservations 2)


testReservationsSeatCapacity = testCase "Reservations must not exceed seat capacity" $ do
  expectRight (parse reservationOf1)
  expectRight (parse reservationOf2)
  expectLeftProblem ReservationsExceedSeatCapacity (parse reservationOf3)
  where 
    parse reservation = refine (Set.singleton reservation) :: Either RefineException (Reservations 2)

    reservationOf1 = mkReservation "18f37138-837b-4497-bf9a-8d0b162a9c86" 1
    reservationOf2 = mkReservation "8dbde54e-ed7e-4449-bc38-7da529de5603" 2
    reservationOf3 = mkReservation "bfe5105b-a0af-4d77-8723-29045b8d811f" 3


-- generators
mkReservationId idString = reservationId
  where 
    Right reservationId = parseReservationId (uuidFromString idString)

mkReservation idString numberOfPassengers = 
  Reservation (mkReservationId idString) reservationNumberOfPassengers
  where
    Right reservationId = parseReservationId (uuidFromString idString)
    Right reservationNumberOfPassengers = refine numberOfPassengers

-- helpers 
uuidFromString = fromJust . UUID.fromString

expectRight = assertBool "expect right" . isRight

expectLeft = assertBool "expect left" . isLeft

expectLeftProblem problem = \case
  Left exception -> Just problem @?= (parseSomeException exception >>= fromException)  
  Right _ -> assertFailure "expect left"

expectOrdEqual lhs rhs = assertBool "expect equal" (compare lhs rhs == EQ)

expectNotOrdEqual lhs rhs = assertBool "expect not equal" (compare lhs rhs /= EQ)

