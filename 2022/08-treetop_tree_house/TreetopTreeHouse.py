from itertools import chain

class Tree:
    def __init__(self, height):
        self.height = height
        self.visible_from = {} # contains directions this tree is visible from - t, b, l, r
        self.scenic_score = 1 # use 1 as base so we can just multiply it

class TreetopTreeHouse:
    def __init__(self, lines):
        self.grid = []
        for l in lines:
            row = [Tree(int(ch)) for ch in l.rstrip()]
            self.grid.append(row)

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return TreetopTreeHouse(lines)

    def solve_part_I(self):
        # check each column from top to bottom
        for c in range(0, len(self.grid[0])):
            max_height = -1
            for r in range(0, len(self.grid)):
                t = self.grid[r][c]
                if t.height > max_height:
                    t.visible_from['t'] = True
                    max_height = t.height

        # check each column from bottom to top
        for c in range(0, len(self.grid[0])):
            max_height = -1
            for r in range(len(self.grid)-1, -1, -1):
                t = self.grid[r][c]
                if t.height > max_height:
                    t.visible_from['b'] = True
                    max_height = t.height
        # check each row from left to right
        for r in range(0, len(self.grid)):
            max_height = -1
            for c in range(0, len(self.grid[0])):
                t = self.grid[r][c]
                if t.height > max_height:
                    t.visible_from['l'] = True
                    max_height = t.height
        # check each row from right to left
        for r in range(0, len(self.grid)):
            max_height = -1
            for c in range(len(self.grid[0])-1, -1, -1):
                t = self.grid[r][c]
                if t.height > max_height:
                    t.visible_from['l'] = True
                    max_height = t.height
        # count number of trees that are visible
        num_visible = 0
        for r in self.grid:
            visible_in_row = [t for t in r if len(t.visible_from) > 0]
            num_visible += len(visible_in_row)
        return num_visible

    def compute_scenic_score(self, row, col):
        its_height = self.grid[row][col].height
        # look N
        visible_N = 0
        for r in range(row-1, -1, -1):
            visible_N += 1
            if self.grid[r][col].height >= its_height:
                break
        # look S
        visible_S = 0
        for r in range(row+1, len(self.grid)):
            visible_S += 1
            if self.grid[r][col].height >= its_height:
                break
        # look E
        visible_E = 0
        for c in range(col+1, len(self.grid[0])):
            visible_E += 1
            if self.grid[row][c].height >= its_height:
                break
        # look W
        visible_W = 0
        for c in range(col-1, -1, -1):
            visible_W += 1
            if self.grid[row][c].height >= its_height:
                break
        return visible_N * visible_S * visible_E * visible_W 

    def solve_part_II(self):
        # iterate over all the trees
        for r in range(0, len(self.grid)):
          for c in range(0, len(self.grid[0])):
              self.grid[r][c].scenic_score = self.compute_scenic_score(r, c)
        return max([ t.scenic_score for t in chain.from_iterable(self.grid)])

if __name__ == '__main__':
    tth = TreetopTreeHouse.read_input_file('input.txt')
    print("{} {}".format(tth.solve_part_I(), tth.solve_part_II()))
