use v6.c;
use strict;
unit module HexDist;

class Hex is export {
    has $.x; # $: scala, .: publicly accessible
    has $.y;
    has $.z;  
}

# parse the input string, and return an array containing the directions
sub parse_input($input) is export {
  my @a = split(",", $input);
  return @a;
};

sub path_to_hash(@path) {
  my %result = (); # empty Hash
  for (@path) {
    %result{$_} += 1; # default is 0, so we don't need to check whether entry already exists
  }
  return %result;
}

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

# for the given hex, returns its distance from the origin (0,0,0)
sub hex_dist_from_origin($hex) {
    return (abs($hex.x) + abs($hex.y) + abs($hex.z)) / 2;
}

sub hex_dist($input) is export {
  my @path_arr = parse_input($input);
  my @path = walk_path(@path_arr);
  return round(hex_dist_from_origin(@path[elems(@path) - 1]));
}

# compute the hex distance for the end square of the given input path
sub hex_dist2($input) is export {
  my @path_str = parse_input($input);
  my %path = path_to_hash(@path_str);
  my $primary_diagonal = 0;
  my $secondary_diagonal = 0;
  my $vertical = 0;
  for %path.kv -> $key, $value {
    given $key {
      when "n"  { $vertical += $value; }
      when "ne" { $primary_diagonal += $value; } 
      when "se" { $secondary_diagonal += $value; }
      when "s"  { $vertical -= $value; }
      when "sw" { $primary_diagonal -= $value; }
      when "nw" { $secondary_diagonal -= $value; }
    }
  } 
  my $x = round((abs($primary_diagonal) + abs($secondary_diagonal)) / 2);
  my $y = round((abs($primary_diagonal) - abs($secondary_diagonal)) / 2 + $vertical);

  # debug output
  #note :%path.perl;
  #note :$vertical.perl;
  #note :$primary_diagonal.perl;
  #note :$secondary_diagonal.perl;
  #note :$x.perl;
  #note :$y.perl;

  # HACK this is not correct - we got the right solution (764) by looking at the graph:
  #          \ n  /
  #        nw +--+ ne
  #          /    \
    #        -+      +-          => 422 n, 342 ne, 99 nw => (422 + 99) n + (342 - 99) ne =>      422 + 342 = 764 overall
    #          \    /
    #        sw +--+ se
    #          / s  \
      #
      #
      return max($x, $y, $primary_diagonal, $secondary_diagonal);
      #return 
    }

    sub MAIN() {
      my @input = "input.txt".IO.lines;
      print(hex_dist(@input));
      exit 1;
    }
