import logging
import functools
from parse import parse

class Monkey:
    def __init__(self, line):
        res = parse("{}: {}", line)
        self.name = res[0]
        # track whether we depend on human input
        self.depends_on_human = (self.name == "humn")
        self.goal_result = None
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
        self.inverse_ops = {"*": "/", "/": "*", "+": "-", "-": "+"}

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
        # first, determine the value we need to achieve for root
        root_result = -1
        self.monkeys["humn"].value = None
        while root_result == -1:
            for _, m in self.monkeys.items():
                if m.value != None:
                    continue
                elif m.name == "humn":
                    continue
                else:
                    lhs = self.monkeys[m.lhs]
                    rhs = self.monkeys[m.rhs]
                    if lhs.depends_on_human or rhs.depends_on_human:
                        logging.info("{} depends on human".format(m.name))
                        m.depends_on_human = True
                    if lhs.value != None and rhs.value != None:
                        val = eval("{} {} {}".format(lhs.value, m.op, rhs.value))
                        m.value = int(val)
                        logging.info("{} has value {}".format(m.name, m.value))
                    if m.name == "root":
                        if lhs.value != None:
                            root_result = lhs.value
                            rhs.goal_result = lhs.value
                            next_monkey = rhs
                            logging.info("root_result: {}, solving for rhs: {}".format(root_result, rhs.name))
                        elif rhs.value != None:
                            root_result = rhs.value
                            lhs.goal_result = rhs.value
                            next_monkey = lhs
                            logging.info("root_result: {}, solving for lhs: {}".format(root_result, lhs.name))
        # now, drill down from root and find the goal result for each node until we reach human
        logging.info("---------------------- starting drill down for goal result -----------")
        result = -1
        while result == -1:
            logging.info("next: {}".format(next_monkey.name))
            if next_monkey.name == "humn":
                return next_monkey.goal_result
            lhs = self.monkeys[next_monkey.lhs]
            rhs = self.monkeys[next_monkey.rhs]
            op = next_monkey.op
            goal_result = next_monkey.goal_result
            logging.info("{}: {} {} {} = {}".format(next_monkey.name, lhs.name, op, rhs.name,goal_result ))
            if lhs.value != None:
                # resolving a op b = c for b needs more explicit handling 
                #   a - b = c => b = a - c
                #   a + b = c => b = c - a
                #   a * b = c => b = c / a
                #   a / b = c => b = a / c
                if op == "-":
                    compute = "{} - {}".format(lhs.value, goal_result)
                elif op == "+":
                    compute = "{} - {}".format(goal_result, lhs.value)
                elif op == "*":
                    compute = "{} / {}".format(goal_result, lhs.value)
                elif op == "/":
                    compute = "{} / {}".format(lhs.value,goal_result)
                goal_result = eval(compute)
                logging.info(" {} = {}, {} = {} = {}".format(lhs.name, lhs.value, rhs.name, compute, goal_result))
                rhs.goal_result = int(goal_result)
                next_monkey = rhs
            elif rhs.value != None:
                # resolving a op b = c for a is simple:
                #   a = c invop b
                compute = "{} {} {}".format(goal_result, self.inverse_ops[op], rhs.value)
                goal_result = eval(compute)
                logging.info(" {} = {}, {} = {} = {}".format(rhs.name, rhs.value, lhs.name, compute, goal_result))
                lhs.goal_result = int(goal_result)
                next_monkey = lhs
            else:
                logging.info("cannot evaluate {}. Exiting.", next_monkey.name)
                return -1
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    solve1 = Solve.read_input_file('input.txt')
    solve2 = Solve.read_input_file('input.txt')
    print("{} {}".format(solve1.solve_part_I(), solve2.solve_part_II()))


