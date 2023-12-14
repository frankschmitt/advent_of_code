import logging
import functools
import numpy as np


# taken from https://stackoverflow.com/a/43870693/610979 and simplified
def dup_row(a, indx):
    return np.insert(a,[indx+1],a[indx],axis=0)

def dup_col(a, indx):
    return np.insert(a,[indx+1],a[:,[indx]],axis=1)

def pretty_print_grid(grid):
    result = "\n".join(["".join([grid[i, j] for j in range(0, len(grid[i]))]) for i in range(0, len(grid))])
    return result

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.grid = np.array([list(line) for line in lines])
        logging.info(f"grid: \n{pretty_print_grid(self.grid)}")

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        # part I: expand it: duplicate empty rows + cols
        # rows
        for i in range(len(self.grid)-1, -1, -1):
            if '#' not in self.grid[i, :]:
                logging.debug(f"duplicating row #{i}: {self.grid[i, :]}")
                self.grid = dup_row(self.grid, i)
        # cols
        for j in range(len(self.grid[0])-1, -1, -1):
            if '#' not in self.grid[:, j]:
                logging.debug(f"duplicating col #{j}: {self.grid[:, j]}")
                self.grid = dup_col(self.grid, j)
        logging.info(f"grid after expansion:\n{pretty_print_grid(self.grid)}")
        # part II: find the galaxies
        galaxies = [pos for pos, x in np.ndenumerate(self.grid) if x == '#']
        result = 0
        for i in range(len(galaxies)):
            for j in range(i+1, len(galaxies)):
                result += abs(galaxies[i][0]-galaxies[j][0]) + abs(galaxies[i][1] - galaxies[j][1])
        return result 

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


