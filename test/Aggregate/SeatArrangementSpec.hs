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
  , testUnregisterUnknownReservation
  , testUnregisterReservation
  , testUnregisterLastReservation
  , testPropose
  , testParseProposals
  , testParseProposalsIncomplete
  , testParseProposalsPartialIncomplete
  , testParseProposalsConflict
  , testProposalsForReservation
  , testProposalsForReservationReservation
  , testProposalsForReservationPassengers
  , testProposalsForReservationNonExisting
  ]


testInitialise = testCase "SeatArrangement is initialising with number of seats" $ do
  case initialise numberOfSeats of
    SomeSeatArrangement (SeatArrangementInitialising) -> assertSuccess
  where 
    Right numberOfSeats = refine 5


testRegisterInitialised = testCase "SeatArrangement can register reservation when initialising" $ do
  case register reservation initialisingSeatArrangement of 
    Right (_ :: SeatArrangement 5 'Registering) -> assertSuccess
    _ -> assertFailure "expect Registering phase"
  where
    reservation = mkReservation "9732d7a0-d75d-4143-aac7-9c028dcec63e" 5


testRegisterInitialisedSeatCapacity = testCase "SeatArrangement cannot register reservations above the seat capacity (intialising)" $ do
  expectLeftValue ReservationsExceedSeatCapacity (register reservation initialisingSeatArrangement)
  where
    reservation = mkReservation "ba9a3ae5-4cdc-40e7-8c93-5bce695fb16d" 6


testRegisterRegistering = testCase "SeatArrangement can register reservation when registering" $ do
  case register reservation registeringSeatArrangement of 
    Right (_ :: SeatArrangement 5 'Registering) -> assertSuccess
    _ -> assertFailure "expect Registering phase"
  where
    reservation = mkReservation "cd64c389-66ca-4a88-93c4-570a23425d18" 3


testRegisterRegisteringSeatCapacity = testCase "SeatArrangement cannot register reservations above the seat capacity (registering)" $ do
  expectLeftValue ReservationsExceedSeatCapacity (register reservation registeringSeatArrangement)
  where
    reservation = mkReservation "d2d082de-2623-4791-b22b-41047c367bd8" 5

testUnregisterUnknownReservation = testCase "SeatArrangement must ignore unregistering unknown reservations" $ do
  case unregister reservation registeringSeatArrangement of 
    RegsteringAfterUnregister (SeatArrangementRegistering (Refined reservations)) -> Set.size reservations @?= 1
    
  where
    reservation = mkReservationId "b8c1429c-8a12-42c5-98b5-f9131d91e8d6"


testUnregisterReservation = testCase "SeatArrangement must remain in Registering phase if there are reservations left" $ do
  case unregister reservationId seatArrangement of 
    RegsteringAfterUnregister (SeatArrangementRegistering (Refined reservations)) -> Set.size reservations @?= 1
    InitialisingAfterUnregister _ -> assertFailure "expect Registering phase"
  where
    Right seatArrangement = register reservation registeringSeatArrangement
    reservation = mkReservation "40e62b5c-dbca-45e2-97c4-94ae07c1e926" 2
    reservationId = mkReservationId "40e62b5c-dbca-45e2-97c4-94ae07c1e926"


testUnregisterLastReservation = testCase "SeatArrangement must transition to Initialise phase if there are no reservations left" $ do
  case unregister reservation registeringSeatArrangement of 
    RegsteringAfterUnregister _ -> assertFailure "expect Initialising phase"
    InitialisingAfterUnregister _ -> assertSuccess
  where
    reservation = mkReservationId "64839257-4367-495d-aaec-05b30b4f7ee9"


testPropose = testCase "SeatArrangement must transition to Proposing on propose" $ do
  case propose registeringSeatArrangement of
    (_ :: SeatArrangement 5 'Proposing) -> assertSuccess
    _ -> assertFailure "expect Proposing phase"


testParseProposals = testCase "SeatArrangement must transition to Confirming after receiving proposals" $ do
  expectRight (parseProposals proposals proposingSeatArrangement)
  where
    Right proposals = refine (Set.fromList [proposal])
    Right proposal = 
      refine $ Bimap.fromList 
        [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 0)
        ,(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 1, mkSeat 1)
        ]


testParseProposalsIncomplete = testCase "SeatArrangement must reject incomplete proposals" $ do
  expectLeftValue ProposalDoesNotCoverAllReservations (parseProposals proposals proposingSeatArrangement)
  where
    Right proposals = refine (Set.fromList [proposal])
    Right proposal = 
      refine $ Bimap.fromList 
        [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 0)
        ]


testParseProposalsPartialIncomplete = testCase "SeatArrangement must reject partial incomplete proposals" $ do
  expectLeftValue ProposalDoesNotCoverAllReservations (parseProposals proposals proposingSeatArrangement)
  where
    Right proposals = refine (Set.fromList [proposal1, proposal2])
    Right proposal1 = 
      refine $ Bimap.fromList 
      [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 0)
      ]
    Right proposal2 = 
      refine $ Bimap.fromList 
      [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 2)
      ,(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 3)
      ]


testParseProposalsConflict = testCase "SeatArrangement must reject conflicting proposals" $ do
  expectLeftValue ProposalDoesNotCoverAllReservations (parseProposals proposals proposingSeatArrangement)
  where
    Right proposals = refine (Set.fromList [proposal])
    Right proposal =
      refine $ Bimap.fromList 
      [(mkPassenger "446e006a-43bd-4739-816d-9d47797816fa" 0, mkSeat 0)
      ,(mkPassenger "446e006a-43bd-4739-816d-9d47797816fa" 1, mkSeat 1)
      ]


testProposalsForReservation = testCase "SeatArrangement must return all proposals for a reservation" $ do
  case proposalsForReservation reservation confirmingSeatArrangement of
    Right (Refined proposals) -> Set.size proposals @?= 2
    Left problem -> assertFailure (show problem)
  where
    reservation = mkReservationId "64839257-4367-495d-aaec-05b30b4f7ee9"


testProposalsForReservationReservation = testCase "SeatArrangement must only return proposals for a reservation" $ do
  case proposalsForReservation reservation1Id confirminSeatArrangement of
    Right (Refined proposals) -> assertBool "includes all passengers" (all includesAllPassengers proposals)
    Left problem -> assertFailure (show problem)
  where
    Right confirminSeatArrangement = parseProposals proposals proposingSeatArrangement
    Right proposingSeatArrangement = propose <$> (register reservation1 initialisingSeatArrangement >>= register reservation2)

    Right proposals = refine (Set.singleton proposal)
    Right proposal =
      refine $ Bimap.fromList 
        [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 0)
        ,(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 1, mkSeat 1)
        ,(mkPassenger "52bff891-4e69-48e0-9900-7c80fe926fa0" 0, mkSeat 2)
        ]

    reservation1Id = mkReservationId "64839257-4367-495d-aaec-05b30b4f7ee9"
    reservation1 = mkReservation "64839257-4367-495d-aaec-05b30b4f7ee9" 2
    reservation2 = mkReservation "52bff891-4e69-48e0-9900-7c80fe926fa0" 1

    includesAllPassengers (Refined proposal) = Bimap.size proposal == 2


testProposalsForReservationPassengers = testCase "SeatArrangement must return all passenger in proposals for a reservation" $ do
  case proposalsForReservation reservation confirmingSeatArrangement of
    Right (Refined proposals) -> assertBool "includes all passengers" (all includesAllPassengers proposals)
    Left reason -> assertFailure "expect proposals"
  where
    reservation = mkReservationId "64839257-4367-495d-aaec-05b30b4f7ee9"
    includesAllPassengers (Refined proposal) = Bimap.size proposal == 2


testProposalsForReservationNonExisting = testCase "SeatArrangement must not return proposals for a reservation if non existing" $ do
  expectLeftValue ProposalsForReservationNotFound (proposalsForReservation reservation confirmingSeatArrangement)
  where
    reservation = mkReservationId "8322add6-b37f-48dd-b5ac-4b3370d19163"

-- confirm
-- prepareBoarding


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
    reservation = mkReservation "64839257-4367-495d-aaec-05b30b4f7ee9" 2

proposingSeatArrangement :: SeatArrangement 5 'Proposing
proposingSeatArrangement = 
  propose registeringSeatArrangement

confirmingSeatArrangement :: SeatArrangement 5 'Confirming
confirmingSeatArrangement = seatArrangement
  where 
    Right seatArrangement = parseProposals proposals proposingSeatArrangement
    Right proposals = refine (Set.fromList [proposal1, proposal2])
    Right proposal1 = 
      refine $ Bimap.fromList 
        [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 0)
        ,(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 1, mkSeat 1)
        ]
    Right proposal2 = 
      refine $ Bimap.fromList 
        [(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 0, mkSeat 2)
        ,(mkPassenger "64839257-4367-495d-aaec-05b30b4f7ee9" 1, mkSeat 3)
        ]


-- generators
mkReservationId idString = reservationId
  where 
    Right reservationId = parseReservationId (uuidFromString idString)

mkReservation idString numberOfPassengers = 
  Reservation (mkReservationId idString) reservationNumberOfPassengers
  where
    Right reservationId = parseReservationId (uuidFromString idString)
    Right reservationNumberOfPassengers = refine numberOfPassengers

mkPassenger idString index = 
  Passenger (mkReservationId idString) index

mkSeat number = seat 
  where
    Right seat = refine number

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