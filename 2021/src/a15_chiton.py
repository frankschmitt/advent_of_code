import logging
import numpy as np
import networkx as nx

class Chiton:
    def node_name(row, col):
        return "{}#{}".format(row, col)

    # shamelessly stolen from Christoph's solution for 09 - Smoke Basin :-)
    def __init__(self, input_data):
        self.map = [[int(x) for x in line.strip()] for line in input_data]
        logging.info("read {} lines".format(len(self.map)))
        # construct the graph; each node is connected to its right and bottom neighbours (and vice versa)
        G = nx.DiGraph()
        for row in range(0, len(self.map)):
            for col in range(0, len(self.map[row])):
                logging.debug("elem at {}/{}: {}".format(row , col, self.map[row][col]))
                # not in bottom-most row? add connection from current to bottom neighbour and vice versa, using the target node's value as edge weight
                if row < len(self.map) - 1:
                  #start = Chiton.node_name(row, col)
                  #end = Chiton.node_name(row+1, col)
                  start = (row, col)
                  end = (row+1, col)
                  G.add_edge(start, end, weight=self.map[row+1][col])
                  G.add_edge(end, start, weight=self.map[row][col])
                # not in right-most row? add connection from current to right neighbour and vice versa, using the target node's value as edge weight
                if col < len(self.map[row]) - 1:
                  #start = Chiton.node_name(row, col)
                  #end = Chiton.node_name(row, col+1)
                  start = (row, col)
                  end = (row, col+1)
                  G.add_edge(start, end, weight=self.map[row][col+1])
                  G.add_edge(end, start, weight=self.map[row][col])
        logging.info("read graph: {}".format(G))   
        path = nx.shortest_path(G, (0,0), (len(self.map)-1, len(self.map[0])-1), weight='weight')
        logging.info("shortest path: {}".format(path))
        # compute weight, ignoring the first entry
        weight = sum([self.map[row][col] for (row,col) in path[1:]])
        logging.info("weight: {}".format(weight))
                  
#>>> G.add_edge('A', 'B', weight=4)
#>>> G.add_edge('B', 'D', weight=2)G
#>>> G.add_edge('A', 'C', weight=
#>>> G.add_edge('C', 'D', weight=4)
#>>> nx.shortest_path(G, 'A', 'D', weight='weight')
#['A', 'B', 'D']

    def solve():
        #f = open("a15_chiton/example_input.txt","r")
        f = open("a15_chiton/input.txt","r")
        #f = open("a15_chiton/small_example_input.txt","r")
        lines = f.readlines()
        ch = Chiton(lines)
        f.close()

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    Chiton.solve()