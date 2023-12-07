import logging
import functools

class Solve:
    def __init__(self, lines):
        times =     [ int(t) for t in lines[0].split()[1:] ]
        distances = [ int(t) for t in lines[1].split()[1:] ]
        logging.info(f"parsed input, times: {times}, distances: {distances}")
        self.times = times
        self.distances = distances

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        return -1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


