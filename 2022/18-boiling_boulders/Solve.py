import logging
import functools

class Solve:
    def __init__(self, lines):
        self.lines = lines

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    """Idea:
        - all_sides = [] (or empty dict)
        - for each cube: store all its sides in all_sides
        - remove duplicates from all_sides (those are the sides that are shared between two cubes)
        - return len(all_sides)
    """
    def solve_part_I(self):
        return -1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


