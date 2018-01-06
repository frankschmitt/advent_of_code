# Synopsis
Solution for the hex-ed challenge, using Perl6 aka Rakudo.

# Overall approach
We implement a basic hex grid with cube coordinates (x,y,z). The distance between two hex tiles a and b is then simply
```
(abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)) / 2
```
See e.g. https://www.redblobgames.com/grids/hexagons/ for a nice introduction to hex grids.

With this approach, obtaining the solution is trivial: 
- walk the path, and keep track of all tiles visited
- final distance = distance of the last tile from the origin
- max distance = max(distance of all visited tiles)

# Usage

## Installation
Install Perl6 according to the instructions on the homepage, e.g. on Mac OS X:
```
brew install rakudo-star
```

## Running the tests
```
prove6 
```

## Solving the puzzle

