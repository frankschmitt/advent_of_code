# vim: ts=4 sw=4 syn=perl :
use v6.c;
use Test; 
use lib 'lib';
use HexDist;

plan 7; # number of tests that we expect to run

is-deeply parse_input("ne,ne,ne"), Hash.new("ne" => 3), "parse_input ne,ne,ne";
is-deeply parse_input("ne,ne,sw,sw"), Hash.new("ne" => 2, "sw" => 2), "parse_input ne,ne,sw,sw";
is-deeply hex_dist("ne,ne,ne"), 3, "distance for ne,ne,ne = 3";
is-deeply hex_dist("ne,ne,sw,sw"), 0, "distance for ne,ne,sw,sw = 0";
is-deeply hex_dist("ne,ne,s,s"),2, "distance for ne,ne,s,s = 2 (se,se)";
is-deeply hex_dist("se,sw,se,sw,sw"), 3, "distance for se,sw,se,sw,sw = 3 (s,s,sw)";

# solutions
sub hex_digest_for_puzzle_input() {
  my @input = "input.txt".IO.lines;
  return hex_dist(@input);
}

is-deeply hex_digest_for_puzzle_input(), 764, "solution for part I";

done-testing;

