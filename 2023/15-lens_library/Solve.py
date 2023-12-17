import logging
import functools as ft

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.steps = lines[0].split(',')
        logging.info(f"parsed steps: {self.steps}")

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        hash_codes = [ft.reduce(lambda acc,ch: ((acc + ord(ch))*17) % 256, step, 0) for step in self.steps]
        logging.info(f"hash_codes: {hash_codes}")
        return sum(hash_codes)

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


