# vim: ts=4 sw=4 syn=perl :
use v6.c;
use Test; 
use lib 'lib';
use HexDist;

plan 4; # number of tests that we expect to run

is-deeply parse_input("ne,ne,ne"), Hash.new("ne" => 3), "parse_input ne,ne,ne";
is-deeply parse_input("ne,ne,sw,sw"), Hash.new("ne" => 2, "sw" => 2), "parse_input ne,ne,sw,sw";
is-deeply hex_dist("ne,ne,ne"), 3, "distance for ne,ne,ne = 3";
is-deeply hex_dist("ne,ne,sw,sw"), 0, "distance for ne,ne,sw,sw = 0";

done-testing;

