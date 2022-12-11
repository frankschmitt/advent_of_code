from parse import parse
from pipe import Pipe

# MonkeyInTheMiddle.py
class Monkey:
    def __init__(self, index, starting_items, operation, test, true_action, false_action):
        self.index = index
        self.items = starting_items 
        self.operation = operation 
        self.test = test
        self.true_action = true_action
        self.false_action = false_action
        self.num_inspections = 0

@Pipe
def split_monkeys(iterable):
    index, starting_items, operation, test, true_action, false_action = None, None, None, None, None, None
    for line in iterable:
        if line == '':
            yield Monkey(index, starting_items, operation, test, true_action, false_action)
            starting_items = []
        else: 
            res = parse("Monkey {:d}:", line)
            if res:
                index = res[0]
            res = parse("  Starting items: {}", line)
            if res:
                starting_items = [int(x) for x in res[0].split(',')]
            res = parse("  Operation: new = old {} {}", line)
            if res:
                operation = (res[0], res[1])
            res = parse("  Test: divisible by {:d}", line)
            if res:
                test = res[0]
            res = parse("    If true: throw to monkey {:d}", line)
            if res:
                true_action = res[0]
            res = parse("    If false: throw to monkey {:d}", line)
            if res:
                false_action = res[0]
    yield Monkey(index, starting_items, operation, test, true_action, false_action)


class MonkeyInTheMiddle:
    def __init__(self, lines):
        self.lines = lines
        self.monkeys = list(lines
                             | split_monkeys
                           )

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return MonkeyInTheMiddle(lines)

    def step(self):
        for m in self.monkeys:
            for i in m.items:
                if m.operation == ('*', 'old'):
                    new_val = i*i
                elif m.operation[0] == '*':
                    new_val = i * m.operation[1]
                elif m.operation[0] == '/':
                    new_val = i / m.operation[1]
                elif m.operation[0] == '-':
                    new_val = i - m.operation[1]
                elif m.operation[0] == '+':
                    new_val = i + m.operation[1]
                new_val = new_val // 3




    def solve_part_I(self):
        for i in range(1, 21):
            self.step()
        sorted_vals = sorted(m.num_inspections for m in self.monkeys)
        return sorted_vals[-1] * sorted_vals[-2]

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    mitm = MonkeyInTheMiddle.read_input_file('input.txt')
    print("{} {}".format(mitm.solve_part_I(), mitm.solve_part_II()))
