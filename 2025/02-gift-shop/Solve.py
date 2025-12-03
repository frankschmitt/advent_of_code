import logging
import functools

class Solve:
    def __init__(self, lines):
        self.lines = lines
        ranges_raw = lines[0].split(',')
        ranges_raw2 = [(rr.split('-')[0], rr.split('-')[1]) for rr in ranges_raw]
        self.ranges = [range(int(low),int(high)+1) for (low,high) in ranges_raw2]
        logging.debug(self.ranges)

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def is_invalid(self, value):
       s = str(value)
       s1 = s[0:len(s)//2]
       s2 = s[len(s)//2:]
       result = s1 == s2
       #logging.debug(f"is_invalid {value} : {s1} ? {s2} = {result}")
       return result


    def solve_part_I(self):
        invalid_ids_list = [ [ v for v in r if self.is_invalid(v)] for r in self.ranges]
        invalid_ids = [v for row in invalid_ids_list for v in row]
        logging.debug(f"invalid ids: {invalid_ids}")
        #return functools.reduce(lambda x,acc: x*acc, invalid_ids, 1)
        return sum(invalid_ids)

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))
