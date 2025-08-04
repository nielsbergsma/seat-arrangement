module Aggregate.SeatArrangement 
  ( SeatArrangementLifecycle(..)
  , SeatArrangement(..)
  , SomeSeatArrangement(..)
  
  -- * Operations related to 'Initialised' phase
  , initialise
  , RegisterReservationProblem
  , CanRegisterReservation(..)

  -- * Operations related to 'Registering' phase
  , UnregisterReservationResult(..)
  , CanUnregisterReservation(..)
  , propose

  -- * Operations related to 'Proposing' phase
  , ProposalProblem(..)
  , parseProposals

  -- * Operations related to 'Confirming' phase
  , ProposalsForReservationProblem(..)
  , proposalsForReservation
  , ConfirmProblem(..)
  , confirm

  -- * Operations related to 'Boarding' phase
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


data SeatArrangementLifecycle = Initialised | Registering | Proposing | Confirming | Boarding

data SeatArrangement (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  -- | Start of lifecycle, when a flight is scheduled, accepts reservations
  SeatArrangementInitialised :: SeatArrangement sc 'Initialised
  -- | Register/unregister reservations
  SeatArrangementRegistering :: Reservations sc -> SeatArrangement sc 'Registering
  -- | Retrieving proposals 
  SeatArrangementProposing   :: Reservations sc -> SeatArrangement sc 'Proposing
  -- | Assignment proposals available, accepting confirmations 
  SeatArrangementConfirming  :: Proposals sc -> ConfirmableAssignments sc -> SeatArrangement sc 'Confirming
  -- | All passengers are assigned a seat, commence boarding 
  SeatArrangementBoarding    :: Assignments sc -> SeatArrangement sc 'Boarding


data SomeSeatArrangement (lc :: SeatArrangementLifecycle) where
  SomeSeatArrangement :: KnownNat sc => SeatArrangement sc lc -> SomeSeatArrangement lc


type RegisterReservationProblem = ReservationsProblem

-- | Register reservation to seat arrangement, applicable in 'Initialised' and 'Registering' phase
class CanRegisterReservation (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  register :: Reservation -> SeatArrangement sc lc -> Either RegisterReservationProblem (SeatArrangement sc 'Registering)


data UnregisterReservationResult (sc :: Nat)
  = RegisteredAfterRemoval (SeatArrangement sc 'Initialised)
  | ProposableAfterRemoval (SeatArrangement sc 'Registering)

-- | Unregisters reservation from seat arrangement, applicable in 'Registering' phase
-- 
-- /note that it can transitition to 'Initialised' phase, when no reservations are present after removal/
class CanUnregisterReservation (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  unregisterReservation :: ReservationId -> SeatArrangement sc lc -> UnregisterReservationResult sc


-- * Operations related to 'Initialised' phase
-- | Creates an empty 'SeatArrangement', based on the 'NumberOfSeats'
initialise :: NumberOfSeats -> SomeSeatArrangement 'Initialised
initialise numberOfSeats =
  case seatCapacity numberOfSeats of 
    SomeNat (Proxy :: Proxy sc) -> SomeSeatArrangement (SeatArrangementInitialised @sc)

instance KnownNat sc => CanRegisterReservation sc 'Initialised where
  register reservation SeatArrangementInitialised = 
    case refine (Set.singleton reservation) of
      Left exception      -> Left (parseReservationsProblem exception)
      Right reservations' -> Right (SeatArrangementRegistering reservations')


-- * Operations related to 'Registering' phase
instance KnownNat sc => CanRegisterReservation sc 'Registering where
  register reservation (SeatArrangementRegistering reservations) = 
    case rerefine (Set.insert reservation) reservations of
      Left exception      -> Left (parseReservationsProblem exception)
      Right reservations' -> Right (SeatArrangementRegistering reservations')


instance KnownNat sc => CanUnregisterReservation sc 'Registering where
  unregisterReservation reservation (SeatArrangementRegistering (Refined reservations)) = 
    case refine reservations' of
      Left _               -> RegisteredAfterRemoval SeatArrangementInitialised
      Right reservations'' -> ProposableAfterRemoval (SeatArrangementRegistering reservations'')
    where 
      reservations' = Set.filter (not . match) reservations
      match (Reservation id' _) = reservation == id'

-- | Ends 'Registering' phase, retrieving proposals 
propose :: KnownNat sc => SeatArrangement sc 'Registering -> SeatArrangement sc 'Proposing
propose (SeatArrangementRegistering reservations) = SeatArrangementProposing reservations

-- * Operations related to 'Proposing' phase
data ProposalProblem 
  = ProposalDoesNotCoverAllReservations
  deriving stock (Eq, Ord)

-- | Parses proposals to 'SeatArrangement', on success the lifecycle transitions to 'Confirming'
parseProposals :: KnownNat sc => Proposals sc -> SeatArrangement sc 'Proposing -> Either ProposalProblem (SeatArrangement sc 'Confirming)
parseProposals proposals (SeatArrangementProposing reservations) 
  | Just problem <- headLeft problems = Left problem
  | Right assignments <- refine confirmableAssignments = Right (SeatArrangementConfirming proposals assignments)
  | otherwise = Left ProposalDoesNotCoverAllReservations
  where
    confirmableAssignments = foldr (\passenger -> Bimap.insert passenger UnassignedSeat) Bimap.empty passengers
    passengers = Set.unions (Set.map passengersOfReservation (unrefine reservations))
    problems = Set.map (parseProposalForReservations reservations) (unrefine proposals)


parseProposalForReservations :: KnownNat sc => Reservations sc -> Proposal sc -> Either ProposalProblem (Proposal sc)
parseProposalForReservations (Refined reservations) proposal@(Refined assignments)
  | all covered reservations = Right proposal
  | otherwise = Left ProposalDoesNotCoverAllReservations
  where 
    covered = all (`Bimap.member` assignments) . passengersOfReservation


-- * Operations related to 'Confirming' phase
data ProposalsForReservationProblem
  = ProposalsForReservationNotFound
  | ProposalsForReservationCorrupt
  deriving stock (Eq, Ord)

-- | Get available propoals for a specific reservation
proposalsForReservation :: KnownNat ps => ReservationId -> SeatArrangement sc 'Confirming -> Either ProposalsForReservationProblem (Proposals ps)
proposalsForReservation id' (SeatArrangementConfirming proposals assignments) 
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
confirm :: (KnownNat ps, KnownNat sc) => ReservationId -> Proposal ps -> SeatArrangement sc 'Confirming -> Either ConfirmProblem (SeatArrangement sc 'Confirming) 
confirm reservation reservationAssignments@(Refined passengerAssignments) arrangement@(SeatArrangementConfirming proposals (Refined confirmableAssignments))
  | not isAvailable = Left ConfirmProposalNotAvailable
  | assignmentsOverlap = Left ConfirmProposalAssignmentsOverlap
  | otherwise =  
    case refine confirmableAssignments' of
      Left exception                 -> Left . ConfirmProposalAssignmentProblem $ parseAssignmentsProblem exception
      Right confirmableAssignments'' -> Right (SeatArrangementConfirming proposals confirmableAssignments'')
  where 
    assignmentsOverlap = 
      Bimap.size confirmableAssignments /= Bimap.size confirmableAssignments'

    confirmableAssignments' = 
      Bimap.fold Bimap.insert 
        confirmableAssignments 
        (Bimap.mapR AssignedSeat passengerAssignments)

    isAvailable = 
      case proposalsForReservation reservation arrangement of
        Left _ -> False
        Right (Refined availableProposals) -> Set.member reservationAssignments availableProposals


-- * Operations related to 'Boarding' phase
data BoardingProblem 
  = BoardingDisallowsUnassignedSeats
  deriving stock (Eq, Ord)

-- | Start boarding
-- 
-- /Precondition: all seats needs to assigned/
board :: KnownNat sc => SeatArrangement sc 'Confirming -> Either BoardingProblem (SeatArrangement sc 'Boarding)
board (SeatArrangementConfirming _ assignments)
  | Bimap.null unconfirmedAssignments = 
      case refine confirmedAssignments of
        Left _          -> Left BoardingDisallowsUnassignedSeats
        Right confirmed -> Right (SeatArrangementBoarding   confirmed)
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
