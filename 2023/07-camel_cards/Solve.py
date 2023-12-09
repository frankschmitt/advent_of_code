import logging
import functools
from collections import Counter
from ordered_enum import OrderedEnum

class HandType(OrderedEnum):
    HIGH_CARD = 3
    ONE_PAIR = 4
    TWO_PAIRS = 5
    THREE_OF_A_KIND = 6
    FULL_HOUSE = 7
    FOUR_OF_A_KIND = 8 
    FIVE_OF_A_KIND = 9 

class Hand:
    # arbitrary values to get ordering right
    CARD_VALUES = { 'A': 14, 'K': 13, 'Q': 12, 'J': 11, 'T': 10, '9': 9, '8': 8, '7': 7, '6': 6, '5': 5, '4': 4, '3': 3, '2' : 2}
    def __init__(self, cards, bid):
        self.bid = bid
        self.cards = cards
        self.counts = Counter(cards)
        # get frequencies, sorted in descending order
        ci = sorted(list(self.counts.values()), reverse=True)
        if ci[0] == 5:
            self.type = HandType.FIVE_OF_A_KIND
        elif ci[0] == 4:
            self.type = HandType.FOUR_OF_A_KIND
        elif ci[0] == 3 and ci[1] == 2:
            self.type = HandType.FULL_HOUSE
        elif ci[0] == 3:
            self.type = HandType.THREE_OF_A_KIND
        elif ci[0] == 2 and ci[1] == 2:
            self.type = HandType.TWO_PAIRS
        elif ci[0] == 2:
            self.type = HandType.ONE_PAIR
        else:
            self.type = HandType.HIGH_CARD
        logging.debug(f"  init hand, cards: {self.cards}, bid: {self.bid}, counts: {self.counts}, type: {self.type}")

    def __str__(self):
        return f"cards: {self.cards}, bid: {self.bid}, type: {self.type}"


    # compare two hands
    def compare(lhs, rhs):
        logging.debug(f"comparing {lhs} with {rhs}")
        if lhs.type < rhs.type:
            logging.debug(f"  lhs.type {lhs.type} < rhs.type {rhs.type} -> -1")
            return -1
        elif lhs.type > rhs.type:
            logging.debug(f"  lhs.type {lhs.type} > rhs.type {rhs.type} -> 1")
            return 1
        else: 
            logging.debug(f"  lhs.type {lhs.type} == rhs.type {rhs.type}, checking cards")
            for i in range(0,5):
                logging.debug(f"  idx = {i}, lhs.card {lhs.cards[i]}, rhs.card {rhs.cards[i]}")
                left  = Hand.CARD_VALUES[lhs.cards[i]]
                right = Hand.CARD_VALUES[rhs.cards[i]]
                logging.debug(f"   left: {left}, right: {right}")
                if left < right:
                    return -1
                elif left > right:
                    return 1
        return 0 

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.hands = [Hand(line.split()[0], int(line.split()[1])) for line in lines ]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        sorted_hands = sorted(self.hands, key=functools.cmp_to_key(Hand.compare))
        logging.debug(f"sorted hands: {[str(h) for h in sorted_hands]}")
        return sum([ (i+1) * sorted_hands[i].bid for i in range(len(sorted_hands)) ])

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


