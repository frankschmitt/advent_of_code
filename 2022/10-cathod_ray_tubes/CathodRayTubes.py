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
        self.output = [ ['.'] * 40 for i in range(0, 7)]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return CathodRayTubes(lines)

    def print_sprite(self, i, msg, instruction, currval):
        pos = i-1
        sprite_range = range(currval-1, currval+2) # correct offset of 1
        if (pos % 40) in sprite_range:
            self.output[pos // 40][pos % 40] = '#'

    def solve_it(self):
        currval = 1
        i = 1
        for ins in self.instructions: 
           self.memory[i] = currval 
           self.print_sprite(i, "begin executing", ins, currval)
           i += 1
           if ins[0] == 'add':
                # wait another cycle, and then perform the change
                self.memory[i] = currval
                self.print_sprite(i, "finish executing", ins, currval)
                i += 1
                currval += ins[1]
        # write result of last instruction
        self.memory[i] = currval
        result = sum(i * self.memory[i] for i in [20, 60, 100, 140, 180, 220])
        # print part II
        for l in self.output:
            print(''.join(l))
        return result

if __name__ == '__main__':
    crt = CathodRayTubes.read_input_file('input.txt')
    #crt = CathodRayTubes.read_input_file('example_input.txt')
    print("{}".format(crt.solve_it()))

