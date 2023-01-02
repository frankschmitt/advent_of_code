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
        forbidden = {}
        for b in self.beacons:
            range_ = abs(b[0]-b[2]) + abs(b[1]-b[3])
            dist_to_row = abs(b[1] - row)
            range_x = range_ - dist_to_row
            logging.info("range for beacon:{} is {}, dist to row {} : {}".format(b, range_, row, dist_to_row))
            for x in range(b[0]-range_x, b[0]+range_x):
              logging.info("  beacon:{} removing {},in row {}".format(b, x, row))
              forbidden[x] = True
        return len(forbidden)

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    # part I: 5181556
    print("{} {}".format(solve.solve_part_I(2000000), solve.solve_part_II()))


