module Aggregate.SeatArrangement 
  ( SeatArrangementLifecycle(..)
  , SeatArrangement(..)
  , SomeSeatArrangement(..)
  
  -- * Operations related to 'Initialising' phase
  , initialise
  , RegisterProblem
  , CanRegister(..)

  -- * Operations related to 'Registering' phase
  , UnregisterResult(..)
  , CanUnregister(..)
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
  , BoardProblem(..)
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


data SeatArrangementLifecycle 
  -- | Initialise lifecycle, when a flight is scheduled, accepts reservations
  = Initialising 
  -- | Register/unregister reservations
  | Registering 
  -- | Retrieving proposals 
  | Proposing 
  -- | Assignment proposals available, accepting confirmations 
  | Confirming 
  -- | All passengers are assigned a seat, commence boarding 
  | Boarding


data SeatArrangement (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  SeatArrangementInitialising :: SeatArrangement sc 'Initialising
  SeatArrangementRegistering  :: Reservations sc -> SeatArrangement sc 'Registering
  SeatArrangementProposing    :: Reservations sc -> SeatArrangement sc 'Proposing
  SeatArrangementConfirming   :: Proposals sc -> ConfirmableAssignments sc -> SeatArrangement sc 'Confirming
  SeatArrangementBoarding     :: Assignments sc -> SeatArrangement sc 'Boarding


data SomeSeatArrangement (lc :: SeatArrangementLifecycle) where
  SomeSeatArrangement :: KnownNat sc => SeatArrangement sc lc -> SomeSeatArrangement lc


type RegisterProblem = ReservationsProblem

-- | Register reservation to seat arrangement, applicable in 'Initialising' and 'Registering' phase
class CanRegister (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  register :: Reservation -> SeatArrangement sc lc -> Either RegisterProblem (SeatArrangement sc 'Registering)


data UnregisterResult (sc :: Nat)
  = InitialisingAfterUnregister (SeatArrangement sc 'Initialising)
  | RegsteringAfterUnregister (SeatArrangement sc 'Registering)

-- | Unregisters reservation from seat arrangement, applicable in 'Registering' phase
-- 
-- /note that it can transitition to 'Initialising' phase, when no reservations are present after unregister/
class CanUnregister (sc :: Nat) (lc :: SeatArrangementLifecycle) where
  unregister :: ReservationId -> SeatArrangement sc lc -> UnregisterResult sc


-- * Operations related to 'Initialising' phase
-- | Creates an empty 'SeatArrangement', based on the 'NumberOfSeats'
initialise :: NumberOfSeats -> SomeSeatArrangement 'Initialising
initialise numberOfSeats =
  case seatCapacity numberOfSeats of 
    SomeNat (Proxy :: Proxy sc) -> SomeSeatArrangement (SeatArrangementInitialising @sc)

instance KnownNat sc => CanRegister sc 'Initialising where
  register reservation SeatArrangementInitialising = 
    case refine (Set.singleton reservation) of
      Left exception      -> Left (parseReservationsProblem exception)
      Right reservations' -> Right (SeatArrangementRegistering reservations')


-- * Operations related to 'Registering' phase
instance KnownNat sc => CanRegister sc 'Registering where
  register reservation (SeatArrangementRegistering reservations) = 
    case rerefine (Set.insert reservation) reservations of
      Left exception      -> Left (parseReservationsProblem exception)
      Right reservations' -> Right (SeatArrangementRegistering reservations')


instance KnownNat sc => CanUnregister sc 'Registering where
  unregister reservation (SeatArrangementRegistering (Refined reservations)) = 
    case refine reservations' of
      Left _               -> InitialisingAfterUnregister SeatArrangementInitialising
      Right reservations'' -> RegsteringAfterUnregister (SeatArrangementRegistering reservations'')
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
data BoardProblem 
  = BoardNotAllowedWithUnassignedSeats
  deriving stock (Eq, Ord)

-- | Start boarding
-- 
-- /Precondition: all seats needs to assigned/
board :: KnownNat sc => SeatArrangement sc 'Confirming -> Either BoardProblem (SeatArrangement sc 'Boarding)
board (SeatArrangementConfirming _ assignments)
  | Bimap.null unconfirmedAssignments = 
      case refine confirmedAssignments of
        Left _          -> Left BoardNotAllowedWithUnassignedSeats
        Right confirmed -> Right (SeatArrangementBoarding confirmed)
  | otherwise = Left BoardNotAllowedWithUnassignedSeats
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
