# RopeBridge.py
from itertools import chain
from functools import reduce
from parse import parse
from copy import deepcopy

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
        self.movements = list(chain.from_iterable([self.instruction_to_movements(l) for l in lines]))

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return RopeBridge(lines)

    # move a rope segment (not the head), and return the new position
    def move_rope_segment(self, predecessor_pos, seg_pos):
        d_x, d_y = 0, 0
        # same x coordinate? check if we need to move in y direction
        if predecessor_pos[0] == seg_pos[0]:
            if predecessor_pos[1] == seg_pos[1] + 2:
                d_y = 1
            elif predecessor_pos[1] == seg_pos[1] - 2:
                d_y = -1
        # same y coordinate? check if we need to move in x direction
        elif predecessor_pos[1] == seg_pos[1]:
            if predecessor_pos[0] == seg_pos[0] + 2:
                d_x = 1
            elif predecessor_pos[0] == seg_pos[0] - 2:
                d_x = -1
        # check if we're adjacent; if not, we need to move diagonally
        elif abs(predecessor_pos[0] - seg_pos[0]) + abs(predecessor_pos[1] - seg_pos[1]) > 2:
            d_x = signum(predecessor_pos[0] - seg_pos[0])
            d_y = signum(predecessor_pos[1] - seg_pos[1])

        new_seg_pos = (seg_pos[0] + d_x, seg_pos[1] + d_y)
        return new_seg_pos


    # execute a single movement
    def move(self, positions_so_far, m):
        last_positions = positions_so_far[-1]
        new_positions = deepcopy(last_positions)
        # move head
        new_positions[0] = (last_positions[0][0] + m[0], last_positions[0][1] + m[1])
        # move remaining segments
        for i in range(1, len(last_positions)):
          new_positions[i] = self.move_rope_segment(new_positions[i-1], last_positions[i])
        return positions_so_far + [new_positions]
        
    def solve_part_I(self):
        initial_positions = [(0,0)] * 2
        positions = reduce(self.move, self.movements, [initial_positions])
        tail_positions = [p[-1] for p in positions]
        return len(set(tail_positions))

    def solve_part_II(self):
        initial_positions = [(0,0)] * 10
        positions = reduce(self.move, self.movements, [initial_positions])
        tail_positions = [p[-1] for p in positions]
        return len(set(tail_positions))

if __name__ == '__main__':
    rb = RopeBridge.read_input_file('input.txt')
    #print("{} {}".format(rb.solve_part_I(), rb.solve_part_II()))
    print("{} {}".format(rb.solve_part_I(), rb.solve_part_II()))
