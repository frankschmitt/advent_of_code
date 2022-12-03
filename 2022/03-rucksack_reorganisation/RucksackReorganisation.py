from pipe import map, izip, groupby

class RucksackReorganisation:
    rucksacks = []

    def __init__(self, rucksacks):
        self.rucksacks = rucksacks

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return RucksackReorganisation(lines)

    def priority(self, ch):
        return ord(ch) - ord('A') + 27 if ch.isupper() else ord(ch) - ord('a') + 1

    def solve_part_I(self):
        return sum(self.rucksacks
                | map(lambda x: x.rstrip()) # remove trailing newline
                | map(lambda x: (x[0 : int(len(x)/2)], x[int(len(x)/2) : len(x)])) # split in middle
                | map(lambda s : set(s[0]).intersection(s[1])) # find common chars
                | map(lambda s: list(s)[0]) # convert 1-character string to first char
                | map(lambda ch: self.priority(ch)) 
                )
                    
    def solve_part_II(self):
        grouped = self.rucksacks | map(lambda x: x.rstrip()) | izip(range(0, len(self.rucksacks))) | groupby(lambda x: int(x[1] / 3))    
        # this was too complicated using pipes; back to plain for loops        
        priority = 0
        for g in grouped:
            s = list(g[1])
            s1 = s[0][0]
            s2 = s[1][0]
            s3 = s[2][0]
            intersect = set(s1).intersection(s2).intersection(s3)
            priority += self.priority(list(intersect)[0])
        return priority


if __name__ == '__main__':
    rr = RucksackReorganisation.read_input_file('input.txt')
    print("{} {}".format(rr.solve_part_I(), rr.solve_part_II()))
