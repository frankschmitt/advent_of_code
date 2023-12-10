# idea: build a directed graph (edge labels = L/R), and walk the path starting from AAA until you reach ZZZ
import logging
import functools
import itertools
import networkx as nx
import regex as re

class Solve:
    def __init__(self, lines):
        G = nx.MultiDiGraph()
        # we add start and end directly so we can keep references to them; the other nodes are added automatically
        self.start = G.add_node('AAA')
        self.end = G.add_node('ZZZ')
        self.lines = lines
        self.path = lines[0]
        re_line = re.compile("^(?P<start>[A-Z]+) = [(](?P<lhs>[A-Z]+), (?P<rhs>[A-Z]+)[)]$")
        for line in lines[2:]:
            m = re_line.match(line)
            if m is None:
                raise Exception(f"wrong line format: {line}")
            G.add_edge(m['start'], m['lhs'], direction="L")
            G.add_edge(m['start'], m['rhs'], direction="R")
        self.G = G
        logging.info(f"parsed, path: {self.path}, graph: {self.G}")

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        curr_node = 'AAA'
        path = [curr_node]
        for p in itertools.cycle(self.path):
            logging.info(f"direction: {p}")
            # get edges for current vertex, including direction, format: (start, end, direction)
            for e in self.G.edges(curr_node, data="direction"):
                logging.info(f"inspecting edge {e}")
                if e[2] == p:
                    path.append(e[1])
                    curr_node = e[1]
                    if e[1] == 'ZZZ':
                        logging.info(f"found path: {path}, length: {len(path)}")
                        return len(path)-1
        return -1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


