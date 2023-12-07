import logging
import functools

class Option:
    def __init__(self, accelerate_ms, run_ms):
        self.accelerate_ms, self.run_ms = accelerate_ms, run_ms
        self.speed = self.accelerate_ms
        self.distance_mm = self.speed * self.run_ms

    def __str__(self):
        return f"Option(acc: {self.accelerate_ms}, run: {self.run_ms}, dist: {self.distance_mm})"

class Solve:
    def __init__(self, lines):
        times =     [ int(t) for t in lines[0].split()[1:] ]
        distances = [ int(t) for t in lines[1].split()[1:] ]
        logging.info(f"parsed input, times: {times}, distances: {distances}")
        self.races = list(zip(times, distances))

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        result = 1
        for r in self.races:
            options = [Option(i, r[0]-i) for i in range(0, r[0]+1)]
            valid_options = [o for o in options if o.distance_mm > r[1] ]
            logging.debug(f"valid option for {r[0]}/{r[1]}: {[str(vo) for vo in valid_options]}")
            result *= len(valid_options)
        return result 

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


