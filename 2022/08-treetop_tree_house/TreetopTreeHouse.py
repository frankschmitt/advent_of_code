class Tree:
    def __init__(self, height):
        self.height = height
        self.visible_from = {} # contains directions this tree is visible from - t, b, l, r

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

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    tth = TreetopTreeHouse.read_input_file('input.txt')
    print("{} {}".format(tth.solve_part_I(), tth.solve_part_II()))
