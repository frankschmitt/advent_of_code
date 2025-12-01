import logging
import functools

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.values = []

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def run(self):
        current = 50
        self.values = [current]
        for line in self.lines:
            val = int(line.replace('L', '-').replace('R', '+'))
            current += val
            self.values.append(current)

    def solve_part_I(self):
        self.run()
        num_zeroes = len([v for v in self.values if v % 100 == 0])
        return num_zeroes

    # 2696: wrong, 3203: wrong
    def solve_part_II(self):
        pairs = list(zip(self.values, self.values[1:]))
        num_crossings = len([1 for a, b in pairs if (a // 100) != (b // 100) or (a % 100 == 0)])
        return num_crossings

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    solve.run()
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


