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
