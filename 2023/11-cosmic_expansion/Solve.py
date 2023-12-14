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

    def solve(self, factor=1e6):
        # part I: expand it: mark row_type (. = no galaxy, # = at least one galaxy)
        # rows
        row_types = []
        for i in range(len(self.grid)-1, -1, -1):
            if '#' not in self.grid[i, :]:
                logging.debug(f"multiplying row #{i}: {self.grid[i, :]}")
                row_types.append('.')
            else:
                row_types.append('#')
        # cols
        col_types = []
        for j in range(len(self.grid[0])-1, -1, -1):
            if '#' not in self.grid[:, j]:
                logging.debug(f"multiplying col #{j}: {self.grid[:, j]}")
                col_types.append('.')
            else:
                col_types.append('#')
        # part II: find the galaxies
        galaxies = [pos for pos, x in np.ndenumerate(self.grid) if x == '#']
        result = 0
        # pairwise comparison / distance computation
        for i in range(len(galaxies)):
            for j in range(i+1, len(galaxies)):
                min_row, max_row = min(galaxies[i][0],galaxies[j][0]), max(galaxies[i][0],galaxies[j][0]) 
                logging.debug(f"computing distance between galaxies {galaxies[i]} and {galaxies[j]}")
                for x in range(min_row, max_row):
                    if row_types[x] == '.': 
                        result += factor
                        logging.debug(f"empty row {x} -> adding {factor}")
                    else:
                        result += 1
                        logging.debug(f"non-empty row {x} -> adding 1")
                min_col, max_col = min(galaxies[i][1],galaxies[j][1]), max(galaxies[i][1],galaxies[j][1]) 
                for y in range(min_col, max_col):
                    if col_types[y] == '.': 
                        result += factor
                        logging.debug(f"empty col {y} -> adding {factor}")
                    else:
                        result += 1
                        logging.debug(f"non-empty col {y} -> adding 1")
        return result 

    def solve_part_I(self):
        return self.solve(2)

    def solve_part_II(self, factor=1000000):
        return self.solve(factor)

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    solve1 = Solve.read_input_file('input.txt')
    solve2 = Solve.read_input_file('input.txt')
    print("{} {}".format(solve1.solve_part_I(), solve2.solve_part_II(1000000)))


