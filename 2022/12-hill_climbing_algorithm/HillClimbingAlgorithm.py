# Idea: build a directed graph
#         vertices = grid squares
#         edges = (v1, v2) where v1 is adjacent to v2 and height(v1) >= height(v2)-1
#       find the shortest path from S to E  

import networkx as nx
import logging

# graph: see 2021/a15_chiton.py

class HillClimbingAlgorithm:

    # check whether we can climb from src to target
    def can_climb(self, src, target):
        if src == 'S':
            src = 'a'
        elif target == 'E':
            target = 'z'
        return ord(src) >= ord(target) - 1

    # adds an edge from start to end in graph G
    def add_edge(self, lines, s_row, s_col, e_row, e_col, G):
        s_height = lines[s_row][s_col]
        e_height = lines[e_row][e_col]
        logging.debug("add edge: ({},{},{}) -> ({},{},{})".format(s_row, s_col, s_height, e_row, e_col, e_height))
        G.add_edge((s_row, s_col, s_height), (e_row, e_col, e_height))

    def __init__(self, lines):
        # build a directed graph from the input. We use (row, col) as node identifiers
        G = nx.DiGraph()
        for row in range(0, len(lines)):
            for col in range(0, len(lines[row])):
                height = lines[row][col]
                if height == 'S':
                    self.start = (row, col, height)
                elif height == 'E':
                    self.end = (row, col, height)
                #print("{}/{} : {}".format(row, col, height))
                # N neighbour max 1 higher? add connection to N
                if row > 0 and self.can_climb(height, lines[row-1][col]):
                    self.add_edge(lines, row, col, row-1, col, G)
                    #G.add_edge((row, col), (row-1, col))
                # S neighbour max 1 higher? add connection to S
                if (row < len(lines)-1) and self.can_climb(height, lines[row+1][col]):
                    self.add_edge(lines, row, col, row+1, col, G)
                    #G.add_edge((row, col), (row+1, col))
                # W neighbour max 1 higher? add connection to W
                if col > 0 and self.can_climb(height, lines[row][col-1]):
                    self.add_edge(lines, row, col, row, col-1, G)
                    # G.add_edge((row, col), (row, col-1))
                # E neighbour max 1 higher? add connection to E
                if (col < len(lines[row])-1) and self.can_climb(height, lines[row][col+1]):
                    self.add_edge(lines, row, col, row, col+1, G)
                    #G.add_edge((row, col), (row, col+1))
        logging.info("read graph: {}".format(G))
        self.G = G


    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return HillClimbingAlgorithm(lines)

    def solve_part_I(self):
        path = nx.shortest_path(self.G, self.start, self.end) 
        logging.info("shortest path: {}".format(path))
        return len(path) - 1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    #logging.basicConfig(level=logging.INFO)
    logging.basicConfig(level=logging.DEBUG)
    # hca = HillClimbingAlgorithm.read_input_file('example_input.txt')
    hca = HillClimbingAlgorithm.read_input_file('input.txt')
    print("{} {}".format(hca.solve_part_I(), hca.solve_part_II()))

