class TuningTrouble:

    def __init__(self, line):
        self.line = line.rstrip()

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return TuningTrouble(lines[0])

    def solve(self, n):
        for i in range(0, len(self.line)-n+1):
            distinct = set(self.line[i:i+n])
            if len(distinct) == n:
                return i+n
        return -1

    def solve_part_I(self):
        return self.solve(4)

    def solve_part_II(self):
        return self.solve(14)

if __name__ == '__main__':
    tt = TuningTrouble.read_input_file('input.txt')
    print("{} {}".format(tt.solve_part_I(), tt.solve_part_II()))
