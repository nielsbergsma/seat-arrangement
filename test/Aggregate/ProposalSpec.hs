module Aggregate.ProposalSpec (tests) where

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
import Aggregate.Proposal


tests :: TestTree
tests = testGroup "Proposal tests"
  [ testProposal
  , testProposals
  , testProposalsNonEmpty
  ]


testProposal = testCase "Proposal must initialise with assignments" $ do
  expectRight actual
  where
    actual = refine assignments :: Either RefineException (Proposal 2)
    assignments = 
      Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, mkSeat 0)
      ,(mkPassenger "7bac6594-5cd9-477c-a16e-fe88096f6c6b" 0, mkSeat 1)
      ]


testProposals = testCase "Proposals must initialise with a set of assignments" $ do
  expectRight proposals
  where
    proposals = refine (Set.fromList [proposal1]) :: Either RefineException (Proposals 2)
    
    Right proposal1 = 
      refine $ Bimap.fromList 
      [(mkPassenger "9ef6ed10-39ed-46c1-9fe6-4acb6c9e5124" 0, mkSeat 0)
      ,(mkPassenger "7bac6594-5cd9-477c-a16e-fe88096f6c6b" 0, mkSeat 1)
      ]


testProposalsNonEmpty = testCase "Proposals must be non-empty" $ do
  expectLeftProblem ProposalsIsEmpty proposals
  where
    proposals = refine Set.empty :: Either RefineException (Proposals 2)


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
