import logging
import functools

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.current = 50
        self.num_zeroes = 0
        self.crossed_zeroes = 0

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def run(self):
        for line in self.lines:
            self.old = self.current
            direction = line[0]
            width = int(line[1:])
            #print(f"Direction: {direction}, Width: {width}")
            if direction == 'L':
                self.current -= width
            elif direction == 'R':
                self.current += width
            else:
                raise ValueError(f"Unknown direction: {direction}")
            if (self.old < 0 and self.current > 0) or (self.old > 0 and self.current < 0):
                self.crossed_zeroes += 1
            if (self.current) > 100:
                self.crossed_zeroes += 1
            msg = f"Moved from {self.old} to {self.current}"
            self.current = self.current % 100
            if self.current == 0:
                self.num_zeroes += 1
                self.crossed_zeroes += 1
            msg += f", , num_zeroes: {self.num_zeroes}, crossed zeroes: {self.crossed_zeroes}"
            print(msg)

    def solve_part_I(self):
        return self.num_zeroes

    def solve_part_II(self):
        return self.crossed_zeroes

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    solve.run()
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


