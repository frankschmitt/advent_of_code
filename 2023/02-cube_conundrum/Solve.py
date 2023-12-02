import logging
import functools as ft
import regex as re

class Set:
    def __init__(self, red, green, blue):
        self.red, self.green, self.blue = red, green, blue

    def __str__(self):
        return f"Set(r: {self.red}, g: {self.green}, b: {self.blue})"

    def is_valid(self):
        return self.red <= 12 and self.green <= 13 and self.blue <= 14


class Game:
    def __init__(self, idx, sets):
        self.idx, self.sets = idx, sets

    def __str__(self):
        return f"Game(idx: {self.idx}, sets: {','.join([str(s) for s in self.sets])}"

    # a game is valid if all of its sets are valid
    def is_valid(self):
        return ft.reduce(lambda acc, set: acc and set.is_valid(), self.sets, True)

    # parse a line, and return the parsed game
    def parse_game(line):
        re_outer = re.compile("Game (?P<idx>[0-9]+): (?P<sets>.*)")
        m_outer = re_outer.match(line)
        if m_outer is None:
            raise Exception(f"line has wrong format: {line}")
        idx = int(m_outer['idx'])
        sets = []
        re_inner = re.compile("\s*(?P<cnt>[0-9]+)\s+(?P<color>blue|red|green)")
        for s in m_outer['sets'].split("; "):
            logging.debug(f"  parsing set: {s}")
            coldict = {"red": 0, "green": 0, "blue": 0}
            for col in s.split(", "):
                logging.debug(f"    parsing color: {col}")
                m_inner = re_inner.match(col)
                if m_inner is None:
                    raise Exception(f"substring has wrong format: {s} -> {col}")
                coldict[m_inner['color']] = int(m_inner['cnt'])
                logging.debug(f"    parsed color: {m_inner['color']} -> {m_inner['cnt']}")
            sets.append(Set(coldict['red'], coldict['green'], coldict['blue']))
        game = Game(idx, sets)
        logging.info(f"parsed line: {line} -> {game}")
        return game


class Solve:
    def __init__(self, lines):
        self.games = [Game.parse_game(line) for line in lines]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        return sum([game.idx for game in self.games if game.is_valid()])

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


