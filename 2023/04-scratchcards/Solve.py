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
        logging.debug(f"parsed '{line}', result: {result}")
        return result

    def num_matching(self):
        result = len(list(set(self.winning_numbers).intersection(self.numbers)))
        return result

    def points(self):
        n = self.num_matching()
        if n >= 1:
            result = 2**(n-1)
        else:
            result = 0
        logging.debug(f"points for {self}: {result}")
        return result


class Solve:
    def __init__(self, lines):
        self.lines = lines
        parsed_cards = [Card.parse_card(line) for line in lines]
        self.cards = { c.idx : c for c in parsed_cards }

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        return sum([c.points() for (idx,c) in self.cards.items()]) 

    def solve_part_II(self):
        # initially, we've got one of each card
        got_cards = { idx: 1 for idx in self.cards}
        # process cards linearly; this is ok because a card with index n only gives us cards with indices > n
        for (idx, c) in self.cards.items():
            count = got_cards[idx]
            debug = f"processing {c}, count: {count}"
            if c.num_matching() == 0:
                debug += ", not winning"
            else:
                debug += ", winning, adding: "
                for i in range(1,c.num_matching()+1):
                    card = self.cards[idx+i]
                    debug += f"{card}, "
                    got_cards[card.idx] += count
            logging.debug(debug)
        return sum([cnt for (idx, cnt) in got_cards.items()])

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


