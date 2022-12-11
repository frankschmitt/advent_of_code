# CathodRayTubes.py

def parse_instruction(ins):
    if ins == 'noop':
        result = 0
    else:
        result = int(ins.split()[1])
    return result

class CathodRayTubes:

    def __init__(self, lines):
        self.lines = lines
        self.memory = [{'instruction': '-', 'delta_instruction': 0, 'x': 1, 'delta_to_apply': 0}] # instruction 0, will be skipped
        self.memory.extend([{'instruction': l, 'delta_instruction': parse_instruction(l), 'x': 1, 'delta_to_apply': 0} for l in lines])
        # extend memory to 230 
        for i in range(len(self.memory), 231):
            self.memory.append({'instruction': '-', 'delta_instruction': 0, 'x': 1, 'delta_to_apply': 0})

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return CathodRayTubes(lines)

    def solve_part_I(self):
        currval = 1
        #for i in range(1, 221):
        for i in range(1, 25):
            self.memory[i+1]['delta_to_apply'] = self.memory[i]['delta_instruction']
            self.memory[i]['x'] = currval 
            currval += self.memory[i]['delta_to_apply']
            print("{} : instruction: {}, delta_instruction: {}, currval: {}, delta_to_apply: {}".format(
                i, self.memory[i]['instruction'], self.memory[i]['delta_instruction'], currval, self.memory[i]['delta_to_apply']))
        result = sum(i * self.memory[i]['x'] for i in [20, 60, 100, 140, 180, 220])
        return result

if __name__ == '__main__':
    crt = CathodRayTubes.read_input_file('input.txt')
    print("{} {}".format(crt.solve_part_I(), crt.solve_part_II()))

