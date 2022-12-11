# CathodRayTubes.py

def parse_instruction(ins):
    if ins == 'noop':
        result = ('noop', 0)
    else:
        result = ('add', int(ins.split()[1]))
    return result

class CathodRayTubes:

    def __init__(self, lines):
        self.lines = lines
        self.instructions = [parse_instruction(l) for l in lines]
        self.memory = [0] * 500 # will store actual values

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return CathodRayTubes(lines)

    def solve_part_I(self):
        currval = 1
        i = 1
        for ins in self.instructions: 
            self.memory[i] = currval 
            i += 1
            if ins[0] == 'add':
                # wait another cycle, and then perform the change
                self.memory[i] = currval
                i += 1
                currval += ins[1]
        # write result of last instruction
        self.memory[i] = currval
        result = sum(i * self.memory[i] for i in [20, 60, 100, 140, 180, 220])
        return result

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    crt = CathodRayTubes.read_input_file('input.txt')
    print("{} {}".format(crt.solve_part_I(), crt.solve_part_II()))

