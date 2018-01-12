# Synopsis
Solution for the packet scanners challenge in Kotlin.

# Overall approach
While part I was pretty straightforward (using a step-by-step simulation for each scanner),
part II was quite tricky; the first couple of approaches:
- for each delay i [0 .. infinity]: stepwise simulation till last scanner, and return the first i with #violations = 0 
    (took several minutes for checking i up to 40k)
- for each delay i [0 .. infinity]: generate the sequence ((range-1)\*2) + depth, and return the first i that is not contained
    in any sequence (took 3 minutes for checking up to 150k)
The final approach is:
- for each delay i [0 .. infinity]: return the first i for which (i + depth\_N) % ((range\_N - 1)\*2) != 0 for all scanners
This solves the puzzle in 400ms :-)

## Installation

## Running the tests
```
gradle test
```

## Solving the puzzle

