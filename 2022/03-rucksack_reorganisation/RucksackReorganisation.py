from pipe import map

class RucksackReorganisation:
    rucksacks = []

    def __init__(self, rucksacks):
        self.rucksacks = rucksacks

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return RucksackReorganisation(lines)

    def priority(ch):
        return -1

    def solve_part_I(self):
        return sum(self.rucksacks
                | map(lambda x: x.rstrip()) # remove trailing newline
                | map(lambda x: (x[0 : int(len(x)/2)], x[int(len(x)/2) : len(x)])) # split in middle
                | map(lambda s : set(s[0]).intersection(s[1])) # find common chars
                | map(lambda s: list(s)[0]) # convert 1-character string to first char
                | map(lambda ch: ord(ch) - ord('A') + 27 if ch.isupper() else ord(ch) - ord('a') + 1)
                )
                    
    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    rr = RucksackReorganisation.read_input_file('input.txt')
    print("{} {}".format(rr.solve_part_I(), rr.solve_part_II()))
