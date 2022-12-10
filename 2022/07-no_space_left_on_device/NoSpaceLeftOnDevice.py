class NoSpaceLeftOnDevice:

    def __init__(self, lines):
        self.lines = [l.rstrip() for l in lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return NoSpaceLeftOnDevice(lines) 

    def solve_part_I(self):
        return -1

    def solve_part_II(self):
        return -1


if __name__ == '__main__':
    ns = NoSpaceLeftOnDevice.read_input_file('input.txt')
    print("{} {}".format(ns.solve_part_I(), ns.solve_part_II()))
