import Test.Tasty
import Test.Tasty.HUnit

import qualified Aggregate.SeatSpec
import qualified Aggregate.ReservationSpec 
import qualified Aggregate.AssignmentSpec 
import qualified Aggregate.ProposalSpec
import qualified Aggregate.SeatArrangementSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "SeatArrangement tests"
  [ Aggregate.SeatSpec.tests
  , Aggregate.ReservationSpec.tests
  , Aggregate.AssignmentSpec.tests
  , Aggregate.ProposalSpec.tests
  , Aggregate.SeatArrangementSpec.tests
  ]
