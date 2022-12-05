from parse import parse

def parse_instruction(s):
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
                if line[1] == '1':
                    mode = 'stack'
                    continue
                else:
                    crate_lines.append(line)
            elif mode == 'stack':
                mode = 'separator'
                continue
            elif mode == 'separator':
                mode = 'instruction'
                continue
            else:
                instruction_lines.append(line)
        # now, actually parse it
        self.stacks = {}
        for cl in crate_lines:
            stack = parse_crate_line(cl)
            for key, value in stack.items():
                if key in self.stacks:
                    self.stacks[key].extend(value)
                else:
                    self.stacks[key] = value
        

        

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return SupplyStacks(lines)
