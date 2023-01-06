import logging
import functools
from parse import parse

class Monkey:
    def __init__(self, line):
        res = parse("{}: {}", line)
        self.name = res[0]
        res2 = parse("{} {} {}", res[1])
        if res2 == None:
            self.lhs = None
            self.op = None
            self.rhs = None
            self.value = int(res[1])
        else:
            self.lhs = res2[0]
            self.op = res2[1]
            self.rhs = res2[2]
            self.value = None
        
class Solve:
    def __init__(self, lines):
        monkey_list = [Monkey(l) for l in lines]
        self.monkeys = {m.name: m for m in monkey_list}

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        result = -1
        while result == -1:
            for _, m in self.monkeys.items():
                if m.value != None:
                    continue
                else:
                    lhs = self.monkeys[m.lhs]
                    rhs = self.monkeys[m.rhs]
                    if lhs.value != None and rhs.value != None:
                        val = eval("{} {} {}".format(lhs.value, m.op, rhs.value))
                        m.value = int(val)
                        if m.name == "root":
                            result = m.value
        return result

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


