# idea: build an undirected graph, and for each node in the connected component of S, compute its distance from S
import logging
import numpy as np
import networkx as nx

class Solve:
    def __init__(self, lines):
        self.lines = lines
        #self.grid = np.array([list(line) for line in lines])
        #logging.info(self.grid)
        logging.info(self.lines)
        # build graph
        G = nx.Graph()
        for row in range(len(lines)):
            for col in range(len(lines[row])):
                ch = lines[row][col]
                if ch == '.':   # . is ground; there is no pipe in this tile.
                    pass
                elif ch == '|': # | is a vertical pipe connecting north and south
                    G.add_edge(f"{row-1}#{col}", f"{row}#{col}")
                    G.add_edge(f"{row}#{col}",   f"{row+1}#{col}")
                elif ch == '-': # - is a horizontal pipe connecting east and west
                    G.add_edge(f"{row}#{col-1}", f"{row}#{col}")
                    G.add_edge(f"{row}#{col}",   f"{row}#{col+1}")
                elif ch == 'L': # L is a 90-degree bend connecting north and east
                    G.add_edge(f"{row-1}#{col}", f"{row}#{col}")
                    G.add_edge(f"{row}#{col}",   f"{row}#{col+1}")
                elif ch == 'J': # J is a 90-degree bend connecting north and west
                    G.add_edge(f"{row-1}#{col}", f"{row}#{col}")
                    G.add_edge(f"{row}#{col-1}", f"{row}#{col}")
                elif ch == '7': # 7 is a 90-degree bend connecting south and west
                    G.add_edge(f"{row+1}#{col}", f"{row}#{col}")
                    G.add_edge(f"{row}#{col-1}", f"{row}#{col}")
                elif ch == 'F': # F is a 90-degree bend connecting south and east
                    G.add_edge(f"{row+1}#{col}", f"{row}#{col}")
                    G.add_edge(f"{row}#{col-1}", f"{row}#{col}")
                elif ch == 'S': # S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has
                    # since S is automagically connected from its neighbours, we don't add any edges, we just note down its position
                    #G.add_edge(f"{row-1}#{col}", f"{row}#{col}")
                    #G.add_edge(f"{row+1}#{col}", f"{row}#{col}")
                    #G.add_edge(f"{row}#{col-1}", f"{row}#{col}")
                    #G.add_edge(f"{row}#{col+1}", f"{row}#{col}")
                    self.start_node = f"{row}#{col}"
                else:
                    raise Exception(f"Unknown character at pos {row}#{col}: {ch}")
        self.G = G
        logging.info("graph")
        for e in self.G.edges:
            logging.info(f"{e}")
        logging.info(f"start node: {self.start_node}")
        ncc = nx.node_connected_component(self.G, self.start_node)
        logging.info(f"ncc: {ncc}")

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve_part_I(self):
        return -1

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


