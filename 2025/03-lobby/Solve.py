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
    
    def solve(self, num_digits):
        res = 0
        for r in self.rows:
            # approach: take the largest integer in allowed positions, note its pos and look for the largest remainder
            # allowed positions: if we have n digits remaining that we need to cram into our output, we're allowed to look only in positions up to length(input)-n
            logging.debug(f"checking row {r}")
            start_idx = 0
            its_res = 0
            for i in range(0, num_digits):
                allowed_row = r.values[start_idx : len(r.values) - num_digits + i + 1]
                logging.debug(f"allowed_row for {i}: {allowed_row}")
                idx = np.argmax(allowed_row)
                start_idx += idx + 1
                its_res = its_res*10 + allowed_row[idx]
            logging.debug(f"max for row {r}: {its_res} for num_digits = {num_digits}")
            res += its_res
        return res


   def solve_part_I(self):
        return self.solve(2)

   def solve_part_II(self):
        return self.solve(12) 

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


