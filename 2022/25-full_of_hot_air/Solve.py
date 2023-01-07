import logging
import functools
from string import ascii_lowercase, digits

snafu_vals = {
        '2': 2,
        '1': 1,
        '0': 0,
        '-': -1,
        '=': -2
}

# convert num to its base 5 representation
def to_base_5(num):
    base = 5
    sep = ','
    glyphs = digits + ascii_lowercase
    if num == 0:
        return '0'
    places = []
    while num:
        n, p = divmod(num, base)
        places.append(p)
        num = n
    return ''.join([glyphs[p] for p in reversed(places)])

def snafu_to_decimal(inp: str) -> int:
    res = 0
    exp = 0
    for i in range(len(inp)-1, -1, -1):
        res += snafu_vals[inp[i]] * 5**exp
        exp += 1
    return int(res) 

def decimal_to_snafu(inp: int) -> str:
    base5 = str(to_base_5(inp))
    logging.info("input: {}, base5: {}".format(inp, base5))
    # now, we need to handle 3 and 4; those aren't allowed in SNAFU. For each 3 we encounter
    # we replace it with a 1= instead; a 4 is replaced by 1-
    carry = 0
    result = ""
    for i in range(len(base5)-1, -1, -1):
        logging.info("  pos {}, inp {}, carry {}".format(i, base5[i], carry))
        if carry == 1:
            logging.info(" CARRY == 1")
            if base5[i] == '0':
                result += '1'
                carry = 0
            elif base5[i] == '1':
                result += '2'
                carry = 0
            elif base5[i] == '2':
                result += '='
                carry = 1
            elif base5[i] == '3':
                result += '-'
                carry = 1
            else:
                result += '0'
                carry = 1
        else:
            logging.info(" CARRY != 1")
            if base5[i] == '3':
                result += "="
                carry = 1
            elif base5[i] == '4':
                result += "-"
                carry = 1
            else:
                result += base5[i]
                carry = 0
        logging.info("  res: {}, carry: {}".format(result, carry))
    if carry == 1:
        logging.info("remaining carry: {}".format(carry))
        result += '1'
    logging.info("decimal_to_snafu: inp={}, base5={}, res={}".format(inp,base5,result[::-1]))
    return result[::-1] # quirky syntax for reversing a string


class Solve:
    def __init__(self, lines):
        self.fuels = [snafu_to_decimal(l) for l in lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        logging.info("sum in decimal: {}".format(sum(self.fuels)))
        return decimal_to_snafu(sum(self.fuels))

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


