import logging
import functools
from parse import parse

class Solve:

    def parse_line(self, line):
        # Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        r = parse("Sensor at x={:d}, y={:d}: closest beacon is at x={:d}, y={:d}", line)
        return [r[0], r[1], r[2], r[3]]

    def __init__(self, lines):
        self.beacons = [self.parse_line(line) for line in lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self, row):
        return -1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(2000000), solve.solve_part_II()))


