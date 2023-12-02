import logging
import functools

class Solve:
    def __init__(self, lines):
        self.lines = lines

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        numbers = [[int(ch) for ch in line if ch.isdigit()] for line in self.lines] 
        first_last = [(a[0], a[-1]) for a in numbers]
        print(first_last)
        return sum([x[0]*10 + x[1] for x in first_last])

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


