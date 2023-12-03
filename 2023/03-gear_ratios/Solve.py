import logging
import functools
import numpy as np

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.grid = np.array( [list(line) for line in lines])
        logging.info(self.grid)
        logging.info(f"rows: {len(self.grid)}, cols: {len(self.grid[0])}")

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    # check whether cell at (row, col) has at least one symbol neighbour)
    def has_symbol_neighbour(self, row, col):
        neighbours = []
        if row > 0:
            if col > 0:
                neighbours.append(self.grid[row-1, col-1])  # NW
            neighbours.append(self.grid[row-1, col]) # N 
            if col < len(self.grid[0])-1:
                neighbours.append(self.grid[row-1, col+1]) # NE
        if col > 0:
            neighbours.append(self.grid[row, col-1]) # W 
        if col < len(self.grid[0])-1:
            neighbours.append(self.grid[row, col+1]) # E 
        if row < len(self.grid)-1: 
            if col > 0:
                neighbours.append(self.grid[row+1, col-1])  # SW
            neighbours.append(self.grid[row+1, col]) # S 
            if col < len(self.grid[0])-1:
                neighbours.append(self.grid[row+1, col+1]) # SE
        return any(x not in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'] for x in neighbours)



    def solve_part_I(self):
        result = 0
        # scan the whole grid for numbers
        for r in range(0, len(self.grid)):
            in_number = False
            has_symbol_neighbour = False
            currval = 0
            for c in range(0, len(self.grid[r])):
                debug = f"pos ({r,c}): {self.grid[r,c]}"
                if self.grid[r,c].isdigit():
                    in_number = True
                    currval = currval*10 + int(self.grid[r,c])
                    has_symbol_neighbour |= self.has_symbol_neighbour(r,c)
                    debug += f" is digit, new currval: {currval}, has_symbol_neighbour: {has_symbol_neighbour}"
                else:
                    debug += " is NO digit"
                    if in_number and has_symbol_neighbour:
                        result += currval
                        debug += "  added, new result: {result}"
                    in_number = False
                    has_symbol_neighbour = False
                    currval = 0
                logging.debug(debug)
        return result 

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


