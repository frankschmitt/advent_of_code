import logging
import functools
from parse import parse
from constraint import Problem

class Sensor:
    def __init__(self, x, y, beacon_x, beacon_y):
        self.x, self.y, self.beacon_x, self.beacon_y = x, y, beacon_x, beacon_y
        self.range_ = abs(x - beacon_x) + abs(y - beacon_y)

    def __eq__(self, other): 
        if not isinstance(other, Sensor):
            # don't attempt to compare against unrelated types
            return NotImplemented

        return self.x == other.x and self.y == other.y and self.beacon_x == other.beacon_x and self.beacon_y == other.beacon_y

    def __str__(self):
        return "sensor: {}/{}, beacon: {}/{}, range: {}".format(self.x, self.y, self.beacon_x, self.beacon_y, self.range_) 


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

    # part I: get number of "forbidden" positions in given row
    # approach:
    #   - start with empty list of forbidden positions (actually, we use a dict to handle duplicates)
    #   - for each beacon: determine its list of forbidden positions, and add it to the overall list of forbidden positions
    #   - return the number of unique positions
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


    def rec_solve_part_II(self, partial_solutions, remaining_sensors):
        if len(remaining_sensors) == 0:
            logging.info("no more sensors remaining - returning {}".format(partial_solutions))
            return partial_solutions
        else:
            hd, tail = remaining_sensors[0], remaining_sensors[1:]
            logging.info("next: {}".format(hd))
            new_solutions = partial_solutions
            # 4 cases: 
            return self.rec_solve_part_II(new_solutions, tail)


    # create a constraint
    # to avoid the late binding problem, we use a function factory; see https://stackoverflow.com/a/3431699/610979
    def make_constraint(self, s_x, s_y, s_range):
        def f(x,y):
            # create the function here
            return abs(x - s_x) + abs(y - s_y) > s_range
        return f

    # idea: - build a system of (#sensors + 2) inequations  
    #           0 <= x <= 4e9
    #           0 <= y <= 4e9
    #           for each sensor s:
    #             abs(s.x - x) + abs(s.y - y) > s.range
    # idea2:
    #   - for each coordinate: keep track of possible positions as an intervall [x_low;x_high], [y_low; y_high]
    #   - start with [0; max_coord]
    #   - for each beacon: handle all 4 different cases, and recursively search the remaining beacons (aggressively pruning empty intervals)
    def solve_part_II(self, max_x, max_y):
        #res = self.rec_solve_part_II([(0,max_x,0,max_y)], self.sensors)
        p = Problem()
        p.addVariable("x", range(0, max_x + 1))
        p.addVariable("y", range(0, max_y + 1))
        for s in self.sensors:
            print("adding rule for sensor {}: abs(x-{}) + abs(y-{}) > {}".format(s, s.x, s.y, s.range_))
            p.addConstraint(self.make_constraint(s.x, s.y, s.range_), ("x", "y"))
        sols = p.getSolutions()
        print("#solutions: {}, first: {}".format(len(sols), sols[0]))

        return int(sols[0]['x'] * 4e6 + sols[0]['y']) 

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    # part I: 5181556
    print("{} {}".format(solve.solve_part_I(2000000), solve.solve_part_II(4000000, 4000000)))


