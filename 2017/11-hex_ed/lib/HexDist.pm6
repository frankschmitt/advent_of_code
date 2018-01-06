use v6.c;
use strict;
unit module HexDist;

class Hex is export {
    has $.x; # $: scalar, .: publicly accessible
    has $.y;
    has $.z;  
}

# parse the input string, and return an array containing the directions
sub parse_input($input) is export {
  my @a = split(",", $input);
  return @a;
};

# walk the path given by the steps; returns the list of squares visited
sub walk_path(@path) is export {
  my @result = [];
  # add the start square
  push(@result, Hex.new(x => 0, y => 0, z => 0));
  my $x = 0;
  my $y = 0; 
  my $z = 0;
  # walk the path, and add each square visited
  for (@path) {
    given $_ {
      when "n"  { $y += 1; $z -= 1; }
      when "ne" { $x += 1; $z -= 1; }
      when "se" { $x += 1; $y -= 1; }
      when "s"  { $y -= 1; $z += 1; }
      when "sw" { $x -= 1; $z += 1; }
      when "nw" { $x -= 1; $y += 1; }
    }
    push(@result, Hex.new(x => $x, y => $y, z => $z));
  }
  return @result;
}

# for the given hex square, returns its distance from the origin (0,0,0)
sub hex_dist_from_origin($hex) {
    return (abs($hex.x) + abs($hex.y) + abs($hex.z)) / 2;
}

# return the last element from the given array
sub last(@arr) {
  return @arr[elems(@arr) - 1];
}

# compute the hex distance for the end square of the given input path
sub hex_dist($input) is export {
  my @path_arr = parse_input($input);
  my @path = walk_path(@path_arr);
  return round(hex_dist_from_origin(last(@path)));
}

