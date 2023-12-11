import logging
import functools as ft
from operator import mul

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.triangles = [ [int(i) for i in line.split() ] for line in lines]
        logging.info(f"triangles: {self.triangles}")

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def find_missing(row):
        triangle = [ row ]
        # part I: compute differences until we get one with only zeroes
        while True:
            current_row = triangle[-1]
            next_row = [ current_row[i+1] - current_row[i] for i in range(0, len(current_row)-1) ]
            triangle.append(next_row)
            if all(x == 0 for x in next_row):
                break
        logging.info(f"triangle before fill: {triangle}")
        # part II: fill the missing spots from the bottom up
        for i in range(len(triangle)-1, 0, -1):
            # new value = sum of last val from current row plus last val from previous row
            logging.debug(f"i: {i}, triangle[i]: {triangle[i]}, triangle[i-1]: {triangle[i-1]}")
            new_val = triangle[i][-1] + triangle[i-1][-1]
            triangle[i-1].append(new_val)
        return triangle[0][-1] 

    def solve_part_I(self):
        missing_vals = [ Solve.find_missing(t) for t in self.triangles ]
        return sum(missing_vals)

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


