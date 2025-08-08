
# Seat Arrangement

An example implementation of type-driven aggregates using Domain-Driven Design (DDD) principles in Haskell.

## Overview

This repository demonstrates how to model and implement domain aggregates in Haskell using a type-driven approach. The seat arrangement domain is used as a practical example to showcase:

The seat arrangement domain models the process of managing seat reservations and assignments for a set of passengers, such as on a train or airplane. It covers the full lifecycle from defining the available seats, registering reservations for groups of passengers, generating and confirming seat assignment proposals, and finally boarding. The system enforces business rules such as seat capacity limits, group adjacency, and prevents invalid or conflicting assignments, all using Haskell's type system to ensure correctness by construction.

- **Aggregates**: Encapsulating business logic and maintaining consistency boundaries
- **Type Safety**: Leveraging Haskell's type system to encode domain constraints
- **Domain Modeling**: Expressing business invariants through types and functions

## Key Concepts

- **Type-Driven Design**: Using Haskell's type system (GADTs, type families, view patterns) to model domain concepts
- **Refined Types**: Encoding invariants directly in types to prevent invalid states at compile time
- **Domain-Driven Design**: Aggregates, value objects, and business rules are modeled explicitly

## Project Structure

- `src/Aggregate/` — Domain logic (Seat, Reservation, Assignment, Proposal, SeatArrangement)
- `test/Aggregate/SeatArrangementSpec.hs` — Property and unit tests for aggregate logic

## How It Works

The aggregate models the lifecycle of a seat arrangement as a series of well-defined phases, each with its own rules and invariants. The transitions between these phases are enforced at the type level, ensuring that only valid operations are possible at any given stage. Here is a breakdown of the process:

1. **Initialising**: The system starts by defining the total number of seats available. This phase ensures that the seat capacity is set and is within allowed bounds.

2. **Registering**: Reservations are added for groups of passengers. The system checks that the total number of reserved seats does not exceed the seat capacity. Each reservation is uniquely identified and associated with a number of passengers.

3. **Proposing**: Once all reservations are registered, the system generates proposals for how passengers can be assigned to seats. Proposals must cover all reservations and respect group adjacency and seat availability constraints. Multiple proposals can be considered, and each is validated for correctness.

4. **Confirming**: A proposal is selected and confirmed. The system checks for conflicts, such as overlapping seat assignments or unavailable seats, and ensures that only valid proposals are accepted. Partial or conflicting confirmations are rejected.

5. **Boarding**: After confirmation, the final seat assignments are established, and passengers can be boarded. The system ensures that all seats are assigned and that no unassigned or conflicting states remain.

Throughout each phase, business rules and invariants are encoded directly in the types using Haskell's type system and the Refined library. This approach makes invalid states unrepresentable and guarantees that only valid transitions and operations are possible, providing a high level of safety and correctness by construction.

## References

- [Refined package](https://hackage.haskell.org/package/refined)
- [Domain-Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design)

## Author

[Niels Bergsma](https://github.com/nielsbergsma)

---

*This project is a learning resource for functional programming, domain-driven design, and type-driven development in Haskell.*