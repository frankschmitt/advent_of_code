import logging
import functools
import numpy as np

class Number:
    def __init__(self, start_row, start_col, end_row, end_col, value):
        self.start_row, self.start_col, self.end_row, self.end_col, self.value = start_row, start_col, end_row, end_col, value

    def __str__(self):
        return f"Number(({self.start_row},{self.start_col}), ({self.end_row},{self.end_col}), {self.value})"

    """checks whether the given cell is adjacent to this number
       Example:
          .....XXXXX....
          .....X123X....
          .....XXXXX....
       All cells marked with X are adjacent to the number 123
    """
    def is_adjacent_to(self, r,c):
        # r = 1, c = 3, start_row = 0, start_col = 0, end_row = 0, end_col = 2
        result = r >= self.start_row-1 and r <= self.end_row+1 and c >= self.start_col-1 and c <= self.end_col+1
        logging.debug(f"  adjacent ({r},{c}) against {str(self)}? : {result}")
        return result

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
                        debug += f"  added, new result: {result}"
                    in_number = False
                    has_symbol_neighbour = False
                    currval = 0
                logging.debug(debug)
            # check if we've got a number adjacent to a symbol at the end of the line; if yes, we still need to add it
            if in_number and has_symbol_neighbour:
                result += currval
                debug += f"  added (end of line), new result: {result}"
                logging.debug(debug)
        return result 

    def solve_part_II(self):
        result = 0
        # scan the whole grid for numbers; for each number we found: store its start and end position and its value
        numbers_found = []
        logging.info("PHASE 1: scanning for numbers")
        for r in range(0, len(self.grid)):
            in_number = False
            currval = 0
            start_row, end_row, start_col, end_col = None, None, None, None
            for c in range(0, len(self.grid[r])):
                debug = f"pos ({r,c}): {self.grid[r,c]}"
                if self.grid[r,c].isdigit():
                    if not in_number:
                        start_row = r
                        start_col = c
                    in_number = True
                    currval = currval*10 + int(self.grid[r,c])
                    debug += f" is digit, new currval: {currval}"
                else:
                    debug += " is NO digit"
                    if in_number:
                        end_row = r
                        end_col = c-1
                        num = Number(start_row, start_col, end_row, end_col, currval)
                        numbers_found.append(num)
                        debug += f"  storing number: {num}"
                    in_number = False
                    start_row, start_col, end_row, end_col = None, None, None, None
                    currval = 0
                logging.debug(debug)
            # check if we've got a number adjacent to a symbol at the end of the line; if yes, we still need to add it
            if in_number:
                end_row = r
                end_col = c
                num = Number(start_row, start_col, end_row, end_col, currval)
                numbers_found.append(num)
                debug += f"  storing number: {num}"
                logging.debug(debug)
        # scan the grid for star symbols
        logging.info("PHASE 2: scanning for stars")
        for r in range(0, len(self.grid)):
            for c in range(0, len(self.grid[r])):
                ch = self.grid[r,c]
                debug = f"pos ({r,c}): {ch}"
                if ch == '*':
                    adjacent_numbers = [n for n in numbers_found if n.is_adjacent_to(r, c)]
                    debug += f" is star, adjacent_numbers: {[str(n) for n in adjacent_numbers]}"
                    if len(adjacent_numbers) == 2:
                        power = adjacent_numbers[0].value * adjacent_numbers[1].value
                        debug += f" ; two numbers -> adding {power}"
                        result += power
                    else:
                        debug += " ; NO two numbers"
                else:
                    debug += " is NO star"
                logging.debug(debug)
        return result 

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


