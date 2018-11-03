library(rlang)

initial_grid = function() {
  '.#.\n..#\n###' 
}

# Check whether the given rule matches the given grid
# The approach is pretty straightforward:
# - check whether the lengths are compatible (i.e. an even-length rule might match only an even-length grid)
# - TODO check all rotations of the pattern against the grid
matches = function(rule, grid) {
  (length(as_bytes(rule[1])) == length(as_bytes(grid[1]))) &&
    (rule[1] == grid[1]) 
}

# Parse a rule from a string.
# We split the string on => and return the result as a simple 2-element vector (input, output)
rule = function(input) {
  unlist(strsplit(input, " => "))
}

grid = function(input) {
  input
}
