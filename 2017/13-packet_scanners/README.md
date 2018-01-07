# Synopsis
Solution for the packet scanners challenge in Kotlin.

# Overall approach
Part I is pretty straightforward; for each step, we
- move the packet
- check if we're getting caught
- move the scanners
We keep track of 
- the current packet position
- the state of each scanner (depth, range and position)
- the violations we've committed so far
The simulation stops when we've passed the final scanner.

# Usage

## Installation

## Running the tests
```
gradle test
```

## Solving the puzzle

