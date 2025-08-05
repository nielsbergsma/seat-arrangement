module Aggregate.SeatSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.TypeLits (someNatVal)
import Data.Maybe (fromJust)
import Data.Either (isRight, isLeft)
import Data.Set qualified as Set
import Extension.Refined (pattern Refined, refine, RefineException)

import Aggregate.Seat


tests :: TestTree
tests = testGroup "Seat tests"
  [ testSeat
  , testNumberOfSeats
  , testAssignableSeatEquality
  , testSeatCapacity
  ]

testSeat = testCase "Seat must range between 0 and 256" $ do
  expectRight (parseSeat 0)
  expectRight (parseSeat 256)
  expectLeft (parseSeat (-1))
  expectLeft (parseSeat 257)
  where 
    parseSeat value = refine value :: Either RefineException Seat
    

testNumberOfSeats = testCase "NumberOfSeats must range between 1 and 256" $ do
  expectRight (parseNumberOfSeats 1)
  expectRight (parseNumberOfSeats 256)
  expectLeft (parseNumberOfSeats 0)
  expectLeft (parseNumberOfSeats 257)
  where 
    parseNumberOfSeats value = refine value :: Either RefineException NumberOfSeats
    

testAssignableSeatEquality = testCase "AssignableSeat must only be equal for Assigned {Seat}" $ do
  expectOrdEqual (AssignedSeat seat1) (AssignedSeat seat1)
  expectNotOrdEqual (AssignedSeat seat1) (AssignedSeat seat2)
  expectNotOrdEqual (UnassignedSeat) (AssignedSeat seat1)
  expectNotOrdEqual (AssignedSeat seat1) (UnassignedSeat)
  expectNotOrdEqual (UnassignedSeat) (UnassignedSeat)
  where
    Right seat1 = refine 1 :: Either RefineException Seat
    Right seat2 = refine 2 :: Either RefineException Seat
    

testSeatCapacity = testCase "seatCapacity must transform NumberOfSeats to SomeNat" $ do
  seatCapacity numberOfSeats @?= expectedSeatCapacity
  where
    Right numberOfSeats = refine 25 :: Either RefineException NumberOfSeats
    Just expectedSeatCapacity = someNatVal 25


-- helpers
expectRight = assertBool "expect right" . isRight

expectLeft = assertBool "expect left" . isLeft

expectOrdEqual lhs rhs = assertBool "expect equal" (compare lhs rhs == EQ)

expectNotOrdEqual lhs rhs = assertBool "expect not equal" (compare lhs rhs /= EQ)
