# Synopsis
Solution for the hex-ed challenge, using Perl6 aka Rakudo.

# Overall approach
We exploit some simple properties of the hex grid:
 - n/s, ne/sw, nw/se cancel each other 
 - ne/nw and se/sw can be shortened to n and s 

Alternatively, we could use a full-blown hex grid implementation with cube coordinates; see e.g. https://www.redblobgames.com/grids/hexagons/ for a nice introduction.

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

