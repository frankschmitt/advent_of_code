import logging
import numpy as np
import networkx as nx

class Chiton:
    def expand_grid(original_grid):
        # init grid with the correct dimensions (all zeroes)
        result = [[0 for col in range(0, len(original_grid[0]) * 5)] for row in range(0, len(original_grid) * 5)]
        # for each cell: set it and its tile-cells in the result 
        #   (admittedly, using np.roll() or something like that would have been much cooler than using four nested for loops :-) 
        for row in range(0, len(original_grid)):
            for col in range(0, len(original_grid[row])):
                for i in range(0, 5):
                    for j in range(0, 5):
                        new_val = original_grid[row][col] + i + j
                        if new_val > 9:
                            new_val = new_val%10 + 1 # wrap around, omit 1
                        result[row + i*len(original_grid)][col + j*len(original_grid[0])] = new_val
        return result

    def __init__(self, grid):
        self.map = grid
        logging.info("read {} lines".format(len(self.map)))
        # construct the graph; each node is connected to its right and bottom neighbours (and vice versa)
        # we use (row, col) as node identifiers; this makes setting the edge weight much more straight-forward
        G = nx.DiGraph()
        for row in range(0, len(self.map)):
            for col in range(0, len(self.map[row])):
                logging.debug("elem at {}/{}: {}".format(row , col, self.map[row][col]))
                # not in bottom-most row? add connection from current to bottom neighbour and vice versa, using the target node's value as edge weight
                if row < len(self.map) - 1:
                  start = (row, col)
                  end = (row+1, col)
                  G.add_edge(start, end, weight=self.map[row+1][col])
                  G.add_edge(end, start, weight=self.map[row][col])
                # not in right-most row? add connection from current to right neighbour and vice versa, using the target node's value as edge weight
                if col < len(self.map[row]) - 1:
                  start = (row, col)
                  end = (row, col+1)
                  G.add_edge(start, end, weight=self.map[row][col+1])
                  G.add_edge(end, start, weight=self.map[row][col])
        logging.info("read graph: {}".format(G))   
        path = nx.shortest_path(G, (0,0), (len(self.map)-1, len(self.map[0])-1), weight='weight')
        logging.info("shortest path: {}".format(path))
        # compute weight, ignoring the first entry
        weight = sum([self.map[row][col] for (row,col) in path[1:]])
        for (row, col) in path[1:]:
            logging.debug("weight at {}/{} : {}".format(row, col, self.map[row][col]))
        logging.info("weight: {}".format(weight))
                  
    def solve():
        #f = open("a15_chiton/example_input.txt","r")
        f = open("a15_chiton/input.txt","r")
        #f = open("a15_chiton/small_example_input.txt","r")
        lines = f.readlines()
        f.close()
        # shamelessly stolen from Christoph's solution for 09 - Smoke Basin :-)
        original_grid = [[int(x) for x in line.strip()] for line in lines]
        expanded_grid = Chiton.expand_grid(original_grid)
        ch1 = Chiton(original_grid)
        ch2 = Chiton(expanded_grid)
        logging.debug(ch2)
        
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    Chiton.solve()