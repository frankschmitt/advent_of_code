import logging
import functools

# Source - https://stackoverflow.com/a/18854817
# Posted by rlms, modified by community. See post 'Timeline' for change history
# Retrieved 2025-12-03, License - CC BY-SA 3.0
def chunkstring(string, length):
   return (string[0+i:length+i] for i in range(0, len(string), length))

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

    # invalid check, part I
    # we simply split the string in the middle and check whether both parts are equal
    def is_invalid_part_I(self, value):
       s = str(value)
       s1 = s[0:len(s)//2]
       s2 = s[len(s)//2:]
       result = s1 == s2
       #logging.debug(f"is_invalid {value} : {s1} ? {s2} = {result}")
       return result

    # invalid check, part II
    # idea: split the string into chunks of the same length and check whether all chunks are equal
    # we have to check chunks of length 1, 2, ..., len(s)//2
    def is_invalid_part_II(self, value):
       s = str(value)
       logging.debug(f"value: {s}, length: {len(s)}, max chunk size: {len(s)//2}")
       for i in range(1,len(s)//2 + 1):
           chunks = list(chunkstring(s,i))
           logging.debug(f"chunks of size {i}: {chunks}")
           if all(c == chunks[0] for c in chunks):
               logging.debug(f"{value} is invalid: {chunks}")
               return True
       logging.debug(f"{value} is valid")
       return False

    def solve_part_I(self):
        invalid_ids_list = [ [ v for v in r if self.is_invalid_part_I(v)] for r in self.ranges]
        invalid_ids = [v for row in invalid_ids_list for v in row]
        logging.debug(f"invalid ids: {invalid_ids}")
        #return functools.reduce(lambda x,acc: x*acc, invalid_ids, 1)
        return sum(invalid_ids)

    def solve_part_II(self):
        invalid_ids_list = [ [ v for v in r if self.is_invalid_part_II(v)] for r in self.ranges]
        invalid_ids = [v for row in invalid_ids_list for v in row]
        logging.debug(f"invalid ids: {invalid_ids}")
        #return functools.reduce(lambda x,acc: x*acc, invalid_ids, 1)
        return sum(invalid_ids)


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))
