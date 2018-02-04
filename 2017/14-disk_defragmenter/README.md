# Synopsis
Solution for the disk defragmenter puzzle, using Java (re-using the KnotHash implementation in Clojure)

# Overall approach
The first part is pretty straightforward - we compute the KnotHash for the 128 input strings, convert each to a hex number and count the 1 bits in that number.
For the second part, we:
- mark each square that carries a 1 bit with an 'X'
- iterate over each 'X' square in the grid 
     check how many of its neighbours already are marked with a number
       if 0: mark the square with the next number
       if 1: use the neighbour's number for the current square
       if > 1: use the lowest of the neighbour's numbers, and forall remaining neighbour's numbers: mark them with this number, as well ("merge" two distinct areas that got connected by the current square)
    
# Usage

## Installation
- install Gradle
```./gradlew build```

## Running the tests
```./gradlew test```

## Solving the puzzle
```./gradlew run```
