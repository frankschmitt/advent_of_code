import logging
import functools
from collections import Counter
from enum import Enum

class HandType(Enum):
    FIVE_OF_A_KIND = 9 
    FOUR_OF_A_KIND = 8 
    FULL_HOUSE = 7
    THREE_OF_A_KIND = 6
    TWO_PAIRS = 5
    ONE_PAIR = 4
    HIGH_CARD = 3

class Hand:
    # arbitrary values to get ordering right
    CARD_VALUES = { 'A': 14, 'K': 13, 'Q': 12, 'J': 11, 'T': 10, '9': 9, '8': 8, '7': 7, '6': 6, '5': 5, '4': 4, '3': 3, '2' : 2}
    def __init__(self, cards):
        self.cards = cards
        self.counts = Counter(cards)
        ci = list(self.counts.items())
        if ci[0] == 5:
            self.type = FIVE_OF_A_KIND
        elif ci[0] == 4:
            self.type = FOUR_OF_A_KIND
        elif ci[0] == 3 and ci[1] == 2:
            self.type = FULL_HOUSE
        elif ci[0] == 3:
            self.type = THREE_OF_A_KIND
        elif ci[0] == 2 and ci[1] == 2:
            self.type = TWO_PAIRS
        elif ci[0] == 2:
            self.type = ONE_PAIR
        else:
            self.type = HIGH_CARD


    # compare two hands
    def compare(lhs, rhs):
        if lhs.type < rhs.type:
            return -1
        elif lhs.type > rhs.type:
            return 1
        else: 
            for i in range(0,5):
                left  = CARD_VALUES[lhs.cards[i]]
                right = CARD_VALUES[rhs.cards[i]]
                if left < right:
                    return -1
                elif left > right:
                    return 1
        return 0 

class Solve:
    def __init__(self, lines):
        self.lines = lines

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        return -1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


