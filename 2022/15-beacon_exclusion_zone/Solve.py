import logging
import functools
from parse import parse

class Sensor:
    def __init__(self, x, y, beacon_x, beacon_y):
        self.x, self.y, self.beacon_x, self.beacon_y = x, y, beacon_x, beacon_y
        self.range_ = abs(x - beacon_x) + abs(y - beacon_y)

    def __eq__(self, other): 
        if not isinstance(other, Sensor):
            # don't attempt to compare against unrelated types
            return NotImplemented

        return self.x == other.x and self.y == other.y and self.beacon_x == other.beacon_x and self.beacon_y == other.beacon_y


class Solve:

    def parse_line(self, line):
        # Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        r = parse("Sensor at x={:d}, y={:d}: closest beacon is at x={:d}, y={:d}", line)
        return Sensor(r[0], r[1], r[2], r[3])

    def __init__(self, lines):
        self.sensors = [self.parse_line(line) for line in lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self, row):
        forbidden = {}
        for s in self.sensors:
            dist_to_row = abs(s.y - row)
            range_x = s.range_ - dist_to_row
            logging.info("range for sensor:{} is {}, dist to row {} : {}".format(s, s.range_, row, dist_to_row))
            for x in range(s.x-range_x, s.x+range_x):
              logging.info("  sensor:{} removing {},in row {}".format(s, x, row))
              forbidden[x] = True
        return len(forbidden)

    def solve_part_II(self, max_x, max_y):
        for x in range(0, max_x+1):
            for y in range(0, max_y+1):
                excluded = False
                for b in self.sensors:
                    pass 
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    # part I: 5181556
    print("{} {}".format(solve.solve_part_I(2000000), solve.solve_part_II(4000000, 4000000)))


