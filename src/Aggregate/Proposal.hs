module Aggregate.Proposal
  ( Proposal
  , Proposals
  , ProposalsProblem(..)
  ) where

import GHC.TypeLits (Nat, KnownNat)
import Control.Exception.Base (Exception, toException)
import Data.Typeable (typeRep)

import Data.Set (Set)
import Data.Set qualified as Set

import Constraint (NotEmpty)
import Extension.Refined (Refined, Predicate(..), success, throwRefineSomeException)

import Aggregate.Assignment (Assignments)


type Proposal (sc :: Nat) = Assignments sc

type Proposals (sc :: Nat) = Refined NotEmpty (Set (Proposal sc))


-- invariants
data ProposalsProblem
  = ProposalsIsEmpty
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

instance KnownNat sc => Predicate NotEmpty (Set (Proposal sc)) where
  validate p proposals
    | not (Set.null proposals) = success
    | otherwise = throwRefineSomeException (typeRep p) (toException ProposalsIsEmpty)