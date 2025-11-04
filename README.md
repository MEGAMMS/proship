# proship
A Battleships Puzzle Solver built in Prolog that can load, visualize, and validate Battleship game configurations.

## Features

- Dynamic Puzzle Loading: 20+ pre-defined puzzles (load_game(1) to load_game(20)) of varying complexity.
- Logic-Based Validation: Checks that all ship placements match the specified row/column counts and fleet composition.
- Flexible Grid Representation: Supports different grid sizes (3×3 up to 10×10) using dynamic predicates.
- Human-Readable Board Rendering: Prints the game board to the console with intuitive ASCII symbols.
- Debug Mode: Toggle verbose debugging messages to trace solver operations.
- Extensible Fleet System: Easily extendable to new ship types or custom puzzles.

## Tech & Concepts Demonstrated
This project applies AI reasoning and constraint satisfaction principles using Prolog’s declarative paradigm:
- Dynamic fact assertion & retraction (assert/1, retractall/1)
- Recursive pattern traversal for row/column validation
- Symbolic grid representation using predicates
- Logical constraint enforcement on fleets and ships
- Reusable modular design for puzzle loading and validation

## Project Structure
```bash
proship/
│
├── demo.pl          # Main Prolog source file
└── README.md               # Documentation
```
