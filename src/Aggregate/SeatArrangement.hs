module Aggregate.SeatArrangement 
  ( SeatArrangementLifecycle(..)
  , SeatArrangement(..)
  , SomeSeatArrangement(..)
  
  -- * Operations related to 'Registered' lifecycle
  , mkSeatArrangement
  , SetReservationProblem
  , CanSetReservation(..)

  -- * Operations related to 'Proposable' lifecycle
  , RemoveReservationResult(..)
  , CanRemoveReservation(..)
  , ParseProposalProblem(..)
  , parseProposals

  -- * Operations related to 'Announcable' lifecycle
  , ProposalsForReservationProblem(..)
  , proposalsForReservation
  , ConfirmProblem(..)
  , confirm

  -- * Operations related to 'Boarable' lifecycle
  , BoardingProblem(..)
  , board
  ) where 

import GHC.TypeLits (Nat, KnownNat, SomeNat(..))
import Data.Typeable (Proxy(..))
import Data.Set qualified as Set
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Extension.Data (sequenceSet, headF, headLeft)

import Aggregate.Proposal (Proposals, Proposal)
import Aggregate.Assignment (Assignments, ConfirmableAssignments, AssignmentsProblem, partitionConfirmableAssignments, parseAssignmentsProblem)
import Aggregate.Seat (Seat, NumberOfSeats, AssignableSeat(..), seatCapacity)
import Aggregate.Reservation (Reservations, Reservation(..), ReservationId, Passenger(..), ReservationsProblem, passengersOfReservation, parseReservationsProblem)

import Extension.Refined (pattern Refined, refine, rerefine, unrefine)


data SeatArrangementLifecycle = Registered | Proposable | Announceable | Boardable


data SeatArrangement (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  SeatArrangementRegistered   :: SeatArrangement sc 'Registered
  SeatArrangementProposable   :: Reservations sc -> SeatArrangement sc 'Proposable
  SeatArrangementAnnounceable :: Proposals sc -> ConfirmableAssignments sc -> SeatArrangement sc 'Announceable
  SeatArrangementBoardable    :: Assignments sc -> SeatArrangement sc 'Boardable


data SomeSeatArrangement (lc :: SeatArrangementLifecycle) where
  SomeSeatArrangement :: KnownNat sc => SeatArrangement sc lc -> SomeSeatArrangement lc


type SetReservationProblem = ReservationsProblem

-- | Add reservation to seat arrangement, applicable in 'Registered' and 'Proposable' lifecycles
class CanSetReservation (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  setReservation :: Reservation -> SeatArrangement sc lc -> Either SetReservationProblem (SeatArrangement sc 'Proposable)


data RemoveReservationResult (sc :: Nat)
  = RegisteredAfterRemoval (SeatArrangement sc 'Registered)
  | ProposableAfterRemoval (SeatArrangement sc 'Proposable)

-- | Removes reservation from seat arrangement, applicable in 'Proposable' lifecycle
-- 
-- /note that it can transitition to 'Registered' lifecycle, when no reservations are present after removal/
class CanRemoveReservation (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  removeReservation :: ReservationId -> SeatArrangement sc lc -> RemoveReservationResult sc


-- * Operations related to 'Registered' lifecycle
-- | Creates an empty 'SeatArrangement', based on the 'NumberOfSeats'
mkSeatArrangement :: NumberOfSeats -> SomeSeatArrangement 'Registered
mkSeatArrangement numberOfSeats =
  case seatCapacity numberOfSeats of 
    SomeNat (Proxy :: Proxy sc) -> SomeSeatArrangement (SeatArrangementRegistered @sc)

instance KnownNat sc => CanSetReservation sc 'Registered where
  setReservation reservation SeatArrangementRegistered = 
    case refine (Set.singleton reservation) of
      Left exception      -> Left (parseReservationsProblem exception)
      Right reservations' -> Right (SeatArrangementProposable reservations')


-- * Operations related to 'Proposable' lifecycle
instance KnownNat sc => CanSetReservation sc 'Proposable where
  setReservation reservation (SeatArrangementProposable reservations) = 
    case rerefine (Set.insert reservation) reservations of
      Left exception      -> Left (parseReservationsProblem exception)
      Right reservations' -> Right (SeatArrangementProposable reservations')


instance KnownNat sc => CanRemoveReservation sc 'Proposable where
  removeReservation reservation (SeatArrangementProposable (Refined reservations)) = 
    case refine reservations' of
      Left _               -> RegisteredAfterRemoval SeatArrangementRegistered
      Right reservations'' -> ProposableAfterRemoval (SeatArrangementProposable reservations'')
    where 
      reservations' = Set.filter (not . match) reservations
      match (Reservation id' _) = reservation == id'


data ParseProposalProblem 
  = ProposalDoesNotCoverAllReservations
  deriving stock (Eq, Ord)


-- | Parses proposals to 'SeatArrangement', on success the lifecycle transitions to 'Announceable'
parseProposals :: KnownNat sc => Proposals sc -> SeatArrangement sc 'Proposable -> Either ParseProposalProblem (SeatArrangement sc 'Announceable)
parseProposals proposals (SeatArrangementProposable reservations) 
  | Just problem <- headLeft problems = Left problem
  | Right assignments <- refine confirmableAssignments = Right (SeatArrangementAnnounceable proposals assignments)
  | otherwise = Left ProposalDoesNotCoverAllReservations
  where
    confirmableAssignments = foldr (\passenger -> Bimap.insert passenger UnassignedSeat) Bimap.empty passengers
    passengers = Set.unions (Set.map passengersOfReservation (unrefine reservations))
    problems = Set.map (parseProposalForReservations reservations) (unrefine proposals)


parseProposalForReservations :: KnownNat sc => Reservations sc -> Proposal sc -> Either ParseProposalProblem (Proposal sc)
parseProposalForReservations (Refined reservations) proposal@(Refined assignments)
  | all covered reservations = Right proposal
  | otherwise = Left ProposalDoesNotCoverAllReservations
  where 
    covered = all (`Bimap.member` assignments) . passengersOfReservation


-- * Operations related to 'Announcable' lifecycle
data ProposalsForReservationProblem
  = ProposalsForReservationNotFound
  | ProposalsForReservationCorrupt
  deriving stock (Eq, Ord)

-- | Get available propoals for a specific reservation
proposalsForReservation :: KnownNat ps => ReservationId -> SeatArrangement sc 'Announceable -> Either ProposalsForReservationProblem (Proposals ps)
proposalsForReservation id' (SeatArrangementAnnounceable proposals assignments) 
  | Just reservation@(Reservation _ (Refined passengers)) <- findReservationInProposals id' proposals = 
      case sequenceSet (Set.map (proposalForReservation reservation) availableProposals) of
        Left problem -> Left problem
        Right reservationProposals -> 
          case refine reservationProposals of 
            Left _ -> Left ProposalsForReservationCorrupt
            Right reservationProposals' -> Right reservationProposals'
        
  | otherwise = Left ProposalsForReservationNotFound
  where 
    availableProposals = Set.filter (assignmentsMemberOfProposal confirmedAssignmentsWithoutReservation) (unrefine proposals)
    confirmedAssignmentsWithoutReservation = Bimap.filter (isNotAssignmentOfReservation id') confirmedAssignments
    confirmedAssignments = fst (partitionConfirmableAssignments assignments)


proposalForReservation :: KnownNat ps => Reservation -> Proposal sc -> Either ProposalsForReservationProblem (Proposal ps)
proposalForReservation (Reservation id' (Refined passengers)) (Refined proposal)
  | Bimap.size assignments == passengers =
    case refine assignments of 
      Right assignments' -> Right assignments'
      Left _             -> Left ProposalsForReservationCorrupt
  | otherwise            =  Left ProposalsForReservationNotFound
  where
    assignments = Bimap.filter (isAssignmentOfReservation id') proposal


data ConfirmProblem 
  = ConfirmProposalNotAvailable
  | ConfirmProposalAssignmentsOverlap
  | ConfirmProposalAssignmentProblem AssignmentsProblem

-- | Confirm seat assignments of reservation
confirm :: (KnownNat ps, KnownNat sc) => ReservationId -> Proposal ps -> SeatArrangement sc 'Announceable -> Either ConfirmProblem (SeatArrangement sc 'Announceable) 
confirm reservation reservationAssignments@(Refined passengerAssignments) arrangement@(SeatArrangementAnnounceable proposals (Refined confirmableAssignments))
  | not isAvailable = Left ConfirmProposalNotAvailable
  | assignmentsOverlap = Left ConfirmProposalAssignmentsOverlap
  | otherwise =  
    case refine confirmableAssignments' of
      Left exception                 -> Left . ConfirmProposalAssignmentProblem $ parseAssignmentsProblem exception
      Right confirmableAssignments'' -> Right (SeatArrangementAnnounceable proposals confirmableAssignments'')
  where 
    assignmentsOverlap 
      = Bimap.size confirmableAssignments /= Bimap.size confirmableAssignments'

    confirmableAssignments' = 
      Bimap.fold Bimap.insert 
        confirmableAssignments 
        (Bimap.mapR AssignedSeat passengerAssignments)

    isAvailable = 
      case proposalsForReservation reservation arrangement of
        Left _ -> False
        Right (Refined availableProposals) -> Set.member reservationAssignments availableProposals


-- * Operations related to 'Boarable' lifecycle

data BoardingProblem 
  = BoardingDisallowsUnassignedSeats
  deriving stock (Eq, Ord)

-- | Start boarding
-- 
-- /Precondition: all seats needs to assigned/
board :: KnownNat sc => SeatArrangement sc 'Announceable -> Either BoardingProblem (SeatArrangement sc 'Boardable)
board (SeatArrangementAnnounceable _ assignments)
  | Bimap.null unconfirmedAssignments = 
      case refine confirmedAssignments of
        Left _          -> Left BoardingDisallowsUnassignedSeats
        Right confirmed -> Right (SeatArrangementBoardable confirmed)
  | otherwise = Left BoardingDisallowsUnassignedSeats
  where 
    (confirmedAssignments, unconfirmedAssignments) = partitionConfirmableAssignments assignments


-- helpers
findReservationInProposals :: ReservationId -> Proposals sc -> Maybe Reservation
findReservationInProposals reservation (Refined proposals) = 
  headF proposals >>= findReservationInProposal reservation


findReservationInProposal :: ReservationId -> Proposal sc -> Maybe Reservation
findReservationInProposal reservation (Refined proposal)
  | Right numberOfPassengers <- refine seatCountOfReservation = Just (Reservation reservation numberOfPassengers)
  | otherwise = Nothing
  where
    seatCountOfReservation = Bimap.size (Bimap.filter (isAssignmentOfReservation reservation) proposal)


assignmentsMemberOfProposal :: Bimap Passenger Seat -> Proposal sc -> Bool
assignmentsMemberOfProposal assignments (Refined proposal) = 
  all (`Bimap.pairMember` proposal) (Bimap.assocs assignments)


isNotAssignmentOfReservation :: ReservationId -> Passenger -> Seat -> Bool
isNotAssignmentOfReservation id' (Passenger reservation _) _ = reservation /= id' 


isAssignmentOfReservation :: ReservationId -> Passenger -> Seat -> Bool
isAssignmentOfReservation id' (Passenger reservation _) _ = reservation == id' 
