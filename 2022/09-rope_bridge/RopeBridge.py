from itertools import chain
from functools import reduce
from parse import parse

def signum(x):
    return 1 if (x > 0) else -1 if (x < 0) else 0

class RopeBridge:

    """ convert an instruction like (U 4) into a set of 1-step movements, i.e.
        input: U 4, output: [(0,-1), (0,-1), (0,-1), (0,-1)]"""
    def instruction_to_movements(self, i):
        x, y = 0, 0
        r = parse("{} {:d}", i)
        if r[0] == 'R':
            x = 1
        elif r[0] == 'L':
            x = -1
        elif r[0] == 'D':
            y = 1
        else:
            y = -1
        return [(x,y)] * r[1]

    def __init__(self, lines):
        self.movements = chain.from_iterable([self.instruction_to_movements(l) for l in lines])
        self.head = (0,0)
        self.tail = (0,0)

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return RopeBridge(lines)


    # execute a single movement
    def move(self, tail_positions, m):
        #print("before applying {}: self.head: {}, self.tail: {}".format(m, self.head, self.tail))
        self.head = (self.head[0] + m[0], self.head[1] + m[1])
        d_x, d_y = 0, 0
        # same x coordinate? check if we need to move in y direction
        if self.head[0] == self.tail[0]:
            if self.head[1] == self.tail[1] + 2:
                d_y = 1
            elif self.head[1] == self.tail[1] - 2:
                d_y = -1
        # same y coordinate? check if we need to move in x direction
        elif self.head[1] == self.tail[1]:
            if self.head[0] == self.tail[0] + 2:
                d_x = 1
            elif self.head[0] == self.tail[0] - 2:
                d_x = -1
        # check if we're adjacent; if not, we need to move diagonally
        elif abs(self.head[0] - self.tail[0]) + abs(self.head[1] - self.tail[1]) > 2:
            d_x = signum(self.head[0] - self.tail[0])
            d_y = signum(self.head[1] - self.tail[1])

        self.tail = (self.tail[0] + d_x, self.tail[1] + d_y)
        
        #print(" after applying {}: self.head: {}, self.tail: {}".format(m, self.head, self.tail))
        return tail_positions + [self.tail]

        
    def solve_part_I(self):
        tail_positions = reduce(self.move, self.movements, [(0,0)])
        print("tail positions: {}".format(tail_positions))
        return len(set(tail_positions))

    def solve_part_II(self):
        return 1

if __name__ == '__main__':
    rb = RopeBridge.read_input_file('input.txt')
    print("{} {}".format(rb.solve_part_I(), rb.solve_part_II()))
