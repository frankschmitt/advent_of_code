# Synopsis
Solution for the disk defragmenter puzzle, using Java (re-using the KnotHash implementation in Clojure)

# Overall approach
The first part is pretty straightforward - we compute the KnotHash for the 128 input strings, convert each to a hex number and count the 1 bits in that number.
For the second part, we:
- build an undirected graph: each occupied cell becomes a vertex, 
  and for each pair of occupied neighbour cells, we add an edge
- compute the number of connected components in the graph
We use the excellent [JGraphT|http://jgrapht.org] library for part II.
  
# Usage

## Installation
- install Gradle
```./gradlew build```

## Running the tests
```./gradlew test```

## Solving the puzzle
```./gradlew run```
