use v6.c;
use strict;
unit module HexDist;

sub parse_input($input) is export {
  my %result = (); # empty Hash
  my @a = split(",", $input);
  for (@a) {
    %result{$_} += 1; # default is 0, so we don't need to check whether entry already exists
  }
  return %result;
};

sub hex_dist($input) is export {
  my %path = parse_input($input);
  return 3;
}


