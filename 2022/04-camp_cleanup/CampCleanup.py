# CampCleanup.py
from pipe import map, filter
import re

class CampCleanup:

    def __init__(self, lines):
        self.lines = lines
        p = re.compile('^([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)$')
        self.pairs = list(self.lines
                         | map(lambda x: x.rstrip())
                         | map(lambda x: p.match(x))
                         | map(lambda x: (int(x.group(1)), int(x.group(2)), int(x.group(3)), int(x.group(4))))
                         )

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return CampCleanup(lines)

    def is_total_overlap(self, x):
        return (x[0] <= x[2] and x[1] >= x[3]) or (x[0] >= x[2] and x[1] <= x[3])

    def is_partial_overlap(self, x):
        return not (x[1] < x[2] or x[0] > x[3]) 

    def solve_part_I(self):
        return sum(self.pairs
                  | filter(lambda x: self.is_total_overlap(x))
                  | map(lambda x: 1)
                  )

    def solve_part_II(self):
        return sum(self.pairs
                  | filter(lambda x: self.is_partial_overlap(x)) 
                  | map(lambda x: 1)
                  )

if __name__ == '__main__':
    cc = CampCleanup.read_input_file('input.txt')
    print("{} {}".format(cc.solve_part_I(), cc.solve_part_II()))
