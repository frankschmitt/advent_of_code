import logging
import functools
import numpy as np

class Solve:

    def add_segments(self, segments):
        for s in segments:
            logging.debug("adding segment {}".format(s))
            self.grid[s[0], s[1]] = '#'
            logging.debug("setting {} to #".format(s))                


    def parse_line(self, line):
        seg_strings = line.split('->')
        segs = [np.fromstring(s, dtype=int, sep=',') for s in seg_strings]
        res = []
        for i in range(0, len(segs)-1):
           x, y = segs[i], segs[i+1] 
           for i in range(min(x[0], y[0]), max(x[0], y[0])+1):
               for j in range(min(x[1], y[1]), max(x[1], y[1])+1):
                  res.append((i, j))
        return res

    def __str__(self):
        s = ""
        for row in range(0, 20):
            s += "{0:>2} ".format(row)
            for col in range(490, 510):
                s += self.grid[col,row]
            s += "\n"
        return s

    def __init__(self, lines):
        # init an empty 1001x1001 grid
        a = [ ['.'] * 1001 for i in range(0, 1001)]
        self.grid = np.asmatrix(a)
        # initialize bottom at 0
        self.bottom = 0
        for l in lines:
            self.add_segments(self.parse_line(l))

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


