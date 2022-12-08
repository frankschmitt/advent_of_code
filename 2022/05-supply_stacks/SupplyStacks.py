from parse import parse
from copy import deepcopy

def parse_instruction_line(s):
    r = parse("move {:d} from {:d} to {:d}", s.rstrip())
    return {'n': int(r[0]), 'source': int(r[1]), 'target': int(r[2])}

def parse_crate_line(s):
    r = {}
    for i in range(1, len(s), 4):
        idx = int(i / 4) + 1
        if s[i] == ' ':
            r[idx] = []
        else:
            r[idx] = [ s[i] ]
    return r

class SupplyStacks:

    def __init__(self, lines):
        crate_lines = []
        instruction_lines = []
        mode = 'crate'
        # read input, and split it into crates and instructions
        for line in lines:
            if mode == 'crate':
                if line[1] == '1': # stack line; we just ignore it 
                    mode = 'separator'
                    continue
                else:
                    crate_lines.append(line)
            elif mode == 'separator':
                mode = 'instruction'
                continue
            else:
                instruction_lines.append(line)
        # now, actually parse it
        self.stacks1 = {}
        for cl in crate_lines:
            stack = parse_crate_line(cl)
            for key, value in stack.items():
                if key in self.stacks1:
                    # terrible hack - apparently, there's no prepend() in Python?
                    tmp = value
                    tmp.extend(self.stacks1[key])
                    self.stacks1[key] = tmp
                else:
                    self.stacks1[key] = value
        self.stacks2 = deepcopy(self.stacks1)
        # instructions
        self.instructions = []
        for il in instruction_lines:
            i = parse_instruction_line(il)
            self.instructions.append(i)

    def execute_instruction(self, ins):
        source = ins['source']
        target = ins['target']
        n = ins['n']
        # part I: LIFO
        for i in range(0, n):
            val = self.stacks1[source].pop()
            self.stacks1[target].append(val)
        # part II: whatever
        vals = self.stacks2[source][-n :]
        self.stacks2[source] = self.stacks2[source][: -n]
        self.stacks2[target].extend(vals)


    def solve_part_I(self):
        for ins in self.instructions:
            self.execute_instruction(ins)
        res = ''
        for idx, s in self.stacks1.items():
            res += s[-1]
        return res

    def solve_part_II(self):
        # this assumes we've already executed the instructions
        res = ''
        for idx, s in self.stacks2.items():
            res += s[-1]
        return res

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return SupplyStacks(lines)


if __name__ == '__main__':
    ss = SupplyStacks.read_input_file('input.txt')
    print("{} {}".format(ss.solve_part_I(), ss.solve_part_II()))
