import logging
import functools
import numpy as np

class Row:
    def __init__(self, s):
        self.values = np.array([int(ch) for ch in s])

    def __str__(self):
        return str(self.values)


class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.rows = [Row(line) for line in self.lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    # idea: take the largest integer in pos 0 .. n-1, note its pos as i and then take the largest one in positions i .. n
    def solve_part_I(self):
        res = 0
        for r in self.rows:
            idx1 = np.argmax(r.values[:-1]) # index of first maximum, but only up to penultimate position
            idx2 = np.argmax(r.values[idx1+1:]) # index of first maximum in remainder
            val = r.values[idx1]*10 + r.values[idx1+1:][idx2]
            #logging.debug(f"max for row {r}: {val} for indices {idx1} / {idx2}")
            res += val
        return res

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


