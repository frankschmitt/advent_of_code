import logging
import functools as ft

class Solve:
    def __init__(self, lines):
        self.lines = lines

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    # input: (original_lines, cleaned_lines); the original_lines are not strictly necessary, but *very* useful for debugging
    def compute(self, input):
        numbers = [(line[0], line[1], [int(ch) for ch in line[1] if ch.isdigit()]) for line in input]
        result = 0
        for (original_line, line, n) in numbers:
            debug = f"{original_line} -> {line} -> {n} -> "
            # add least two digits found -> add first and last one
            if len(n) >= 2:
                debug += f"at least zwei digits found: {n[0]}, {n[-1]} -> {n[0]*10 + n[-1]}"
                result += n[0]*10 + n[-1]
            # only a single digit found -> add that one
            elif len(n) == 1:
                debug += f"eine digit found:{n[0]} -> {n[0]*11}"
                result += n[0]*11
            # no digit found: don't add anything
            else:
                debug += f"no digit found -> 0"
            logging.debug(debug)
        return result


    def solve_part_I(self):
        return self.compute(list(zip(self.lines, self.lines)))

    def solve_part_II(self):
        repls = ('one', '1'), ('two', '2'), ('three', '3'), ('four', '4'), ('five', '5'), ('six', '6'), ('seven', '7'), ('eight', '8'), ('nine', '9')
        lines = [] 
        for line in self.lines:
            cleaned = ""
            i = 0
            # check each replacement; if it matches, we simply add the corresponding digit at the current position, but leave the rest as-is to ensure
            #   we handle overlapping digit strings correctly (e.g. 1eightwo -> [1,8,2], *not* [1,8])
            while i < len(line):
                replaced=False
                for r in repls:
                    if i + len(r[0]) <= len(line) and line[i:i+len(r[0])] == r[0]:
                        cleaned += r[1]
                cleaned += line[i]
                i += 1
            lines.append(cleaned)
        return self.compute(list(zip(self.lines, lines)))

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


