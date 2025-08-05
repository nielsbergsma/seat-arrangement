module Aggregate.SeatArrangementSpec (tests) where

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
import Unsafe.Coerce (unsafeCoerce)
import Extension.Refined (pattern Refined, RefineException, refine, parseSomeException)

import Aggregate.Seat
import Aggregate.Reservation 
import Aggregate.Assignment
import Aggregate.Proposal
import Aggregate.SeatArrangement


tests :: TestTree
tests = testGroup "SeatArrangement tests"
  [ testInitialise
  , testRegisterInitialised
  , testRegisterInitialisedSeatCapacity
  , testRegisterRegistering
  , testRegisterRegisteringSeatCapacity
  ]


testInitialise = testCase "SeatArrangement is initialising with number of seats" $ do
  case initialise numberOfSeats of
    SomeSeatArrangement (SeatArrangementInitialising) -> assertSuccess
  where 
    Right numberOfSeats = refine 5


testRegisterInitialised = testCase "SeatArrangement can register reservation when initialising" $ do
  case register reservation initialisingSeatArrangement of 
    Right (seatArrangement' :: SeatArrangement 5 'Registering) -> assertSuccess
    _ -> assertFailure "expect registering phase"
  where
    reservation = mkReservation "9732d7a0-d75d-4143-aac7-9c028dcec63e" 5


testRegisterInitialisedSeatCapacity = testCase "SeatArrangement cannot register reservations above the seat capacity (intialising)" $ do
  expectLeftValue ReservationsExceedSeatCapacity (register reservation initialisingSeatArrangement)
  where
    reservation = mkReservation "ba9a3ae5-4cdc-40e7-8c93-5bce695fb16d" 6


testRegisterRegistering = testCase "SeatArrangement can register reservation when registering" $ do
  case register reservation registeringSeatArrangement of 
    Right (seatArrangement' :: SeatArrangement 5 'Registering) -> assertSuccess
    _ -> assertFailure "expect registering phase"
  where
    reservation = mkReservation "cd64c389-66ca-4a88-93c4-570a23425d18" 4


testRegisterRegisteringSeatCapacity = testCase "SeatArrangement cannot register reservations above the seat capacity (registering)" $ do
  expectLeftValue ReservationsExceedSeatCapacity (register reservation registeringSeatArrangement)
  where
    reservation = mkReservation "d2d082de-2623-4791-b22b-41047c367bd8" 5


-- unregister
-- propose
-- parseProposals
-- proposalsForReservation
-- confirm
-- board


-- fixtures
initialisingSeatArrangement :: SeatArrangement 5 'Initialising
initialisingSeatArrangement = 
  case initialise numberOfSeats of
    SomeSeatArrangement arrangement -> unsafeCoerce arrangement
  where 
    Right numberOfSeats = refine 5

registeringSeatArrangement :: SeatArrangement 5 'Registering
registeringSeatArrangement = seatArrangement
  where 
    Right seatArrangement = register reservation initialisingSeatArrangement
    reservation = mkReservation "64839257-4367-495d-aaec-05b30b4f7ee9" 1

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

expectLeftValue problem = \case
  Left problem' -> problem @?= problem'
  Right _ -> assertFailure "expect left"
 
expectLeftProblem problem = \case
  Left exception -> Just problem @?= (parseSomeException exception >>= fromException)  
  Right _ -> assertFailure "expect left"

assertSuccess = assertBool "expect success" True