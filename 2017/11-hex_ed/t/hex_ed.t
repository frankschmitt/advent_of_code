# vim: ts=4 sw=4 syn=perl :
use v6.c;
use Test; 
use lib 'lib';
use HexDist;

plan 9; # number of tests that we expect to run

# tests for parse_input
is-deeply parse_input("ne,ne,ne"), [ "ne", "ne", "ne" ], "parse_input ne,ne,ne";
is-deeply parse_input("ne,ne,sw,sw"), [ "ne", "ne", "sw", "sw" ],  "parse_input ne,ne,sw,sw";

# tests for walk
is-deeply walk_path([]), [Hex.new(x => 0, y => 0, z => 0)], "walk_path should contain start square"; 
is-deeply walk_path(["n", "ne", "se", "s", "sw", "nw"]), 
                    [ Hex.new(x => 0, y => 0, z => 0)   # start
                    , Hex.new(x => 0, y => 1, z => -1)  # n
                    , Hex.new(x => 1, y => 1, z => -2)  # ne
                    , Hex.new(x => 2, y => 0, z => -2)  # se 
                    , Hex.new(x => 2, y => -1, z => -1) # s
                    , Hex.new(x => 1, y => -1, z => 0)   # sw
                    , Hex.new(x => 0, y => 0, z => 0)  # nw
                    ], 
                    "walk_path should handle single steps";

# tests for hex_dist
is-deeply hex_dist("ne,ne,ne"), 3, "distance for straight diagonal ne,ne,ne = 3";
is-deeply hex_dist("ne,ne,sw,sw"), 0, "distance for walking forth and back ne,ne,sw,sw = 0";
is-deeply hex_dist("ne,ne,s,s"),2, "distance for first diagonal and vertical: ne,ne,s,s = 2 (se,se)";
is-deeply hex_dist("se,sw,se,sw,sw"), 3, "distance for mixed diagonals and vertical: se,sw,se,sw,sw = 3 (s,s,sw)";

# solutions
sub hex_digest_for_puzzle_input() {
  my @input = "input.txt".IO.lines;
  return hex_dist(@input);
}

is-deeply hex_digest_for_puzzle_input(), 764, "solution for part I";

done-testing;

