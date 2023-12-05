import logging
import functools
import regex as re

class Card:
    def __init__(self, idx, winning_numbers, numbers):
        self.idx, self.winning_numbers, self.numbers = idx, winning_numbers, numbers

    def __str__(self):
        return f"Card({self.idx}: {self.winning_numbers} | {self.numbers}"

    def parse_card(line):
        r = re.compile("Card\s+(?P<idx>[0-9]+): (?P<winning_numbers>.+)[|](?P<numbers>.+)")
        m = r.match(line)
        if m is None:
            raise Exception(f"Line has wrong format: {line}")
        idx = int(m['idx'])
        winning_numbers = [int(s) for s in m['winning_numbers'].split()]
        numbers = [int(s) for s in m['numbers'].split()]
        result = Card(idx, winning_numbers, numbers)
        logging.info(f"parsed '{line}', result: {result}")
        return result

    def points(self):
        num_common = len(list(set(self.winning_numbers).intersection(self.numbers)))
        if num_common >= 1:
            result = 2**(num_common-1)
        else:
            result = 0
        logging.info(f"points for {self}: {result}")
        return result


class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.cards = [Card.parse_card(line) for line in lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        return sum([c.points() for c in self.cards]) 

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


