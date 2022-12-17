import logging

class Pair:
    def __init__(self, idx, left, right):
        self.idx = idx
        # our input is valid Python code (a list definition) - shamelessly exploit this
        self.left = eval(left)
        self.right = eval(right)

    # result: 0 = eq, -1: left < right, 1: left > right
    def compare_lists(self, left, right):
        logging.debug("compare_lists: {} <=> {}".format(left, right))
        if len(left) == 0: 
            if len(right) == 0:
                return 0 # both ran out of items; undecided
            else:
                return -1 # left ran out of items - ok
        elif len(right) == 0:
            return 1 # right ran out of items first - not ok
        elif type(left[0]) == type(0):
            if type(right[0]) == type(0):
                logging.debug("both ints: {} and {}".format(left[0], right[0]))
                if left[0] < right[0]:
                    logging.debug("{} < {}".format(left[0], right[0]))
                    return -1
                elif left[0] > right[0]:
                    logging.debug("{} > {}".format(left[0], right[0]))
                    return 1
                else:
                    logging.debug("{} == {}, comparing tails".format(left[0], right[0]))
                    return self.compare_lists(left[1:], right[1:])
            else:
                res = self.compare_lists([left[0]], right[0]) 
                if res in (-1, 1):
                    return res
                else:
                    return self.compare_lists(left[1:], right[1:])
        else: # left starts with list
            if type(right[0]) == type(0): # right is int - expand it, and continue
                res = self.compare_lists([left[0]], [right[0]]) 
                if res in (-1, 1):
                    return res
                else:
                    return self.compare_lists(left[1:], right[1:])
            else: # both are lists
                res = self.compare_lists(left[0], right[0]) 
                if res in (-1, 1):
                    return res
                else:
                    return self.compare_lists(left[1:], right[1:])

    def is_in_right_order(self):
        result = self.compare_lists(self.left, self.right)
        logging.info("{}: {} <=> {} : {}".format(self.idx, self.left, self.right, result))
        return result in (-1, 0) 

class DistressSignal:

    def __init__(self, lines):
        self.pairs = []
        idx = 1
        for i in range(0, len(lines), 3):
            left = lines[i]
            right = lines[i+1]
            self.pairs.append(Pair(idx, left, right))
            idx += 1
        logging.info("read input")
        logging.debug("\n".join(map(lambda p: "({} : {} <=> {}".format(p.idx, p.left, p.right), self.pairs)))

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return DistressSignal(lines)

    def solve_part_I(self):
        return sum(p.idx for p in self.pairs if p.is_in_right_order()) 

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    ds = DistressSignal.read_input_file('input.txt')
    #ds = DistressSignal.read_input_file('example_input.txt')
    print("{} {}".format(ds.solve_part_I(), ds.solve_part_II()))


