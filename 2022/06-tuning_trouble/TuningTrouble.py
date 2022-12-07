class TuningTrouble:

    def __init__(self, line):
        self.line = line.rstrip()

    def read_input_file(filename):
        with open(filename) as f:
            lines = f.readlines()
        return TuningTrouble(lines[0])

    def solve(self, n):
        for i in range(0, len(self.line)-n+1):
            duplicate = False
            for j in range(i+1, i+n-1):
                if self.line[i] == self.line[j]:
                    duplicate = True 
                    break
            if duplicate == False:
                return i+n
        return -1

    def solve_part_I_old(self):
        for i in range(0, len(self.line)-3):
            a, b, c, d = self.line[i], self.line[i+1], self.line[i+2], self.line[i+3]
            if (a != b) and (a != c) and (a != d) and (b != c) and (b != d) and (c != d):
                return i+4
        return -1

    def solve_part_I(self):
        return self.solve(4)

if __name__ == '__main__':
    #tt = TuningTrouble.read_input_file('example_input.txt')
    tt = TuningTrouble.read_input_file('input.txt')
    print("{} {}".format(tt.solve_part_I(), tt.solve_part_I_old()))
