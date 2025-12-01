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
        current = 50
        values = [current]
        for line in self.lines:
            val = int(line.replace('L', '-').replace('R', '+'))
            current += val
            values.append(current)
        num_zeroes = len([v for v in values if v % 100 == 0])
        return num_zeroes

    # brute-force dumb solution: simulate every teeny tiny step
    def solve_part_II(self):
        current = 50
        num_crossings = 0
        for line in self.lines:
            val = int(line.replace('L', '-').replace('R', '+'))
            if val > 0:
                for i in range(0, val):
                    current += 1
                    if current == 100:
                        num_crossings += 1
                        current = 0
            elif val < 0:
                for i in range(0, -val):
                    current -= 1
                    if current == 0:
                        num_crossings += 1
                    # wrap around (for -1 and not for 0, because we might hit zero exactly and get a R afterwards which
                    #  would generate a wrong result)
                    elif current == -1:
                        current = 99
        return num_crossings

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


