# idea: build an undirected graph, and for each node in the connected component of S, compute its distance from S
import logging
import numpy as np
import networkx as nx

# for printing
CSI = "\x1B["

#print(CSI+"31;10m" + "Colored Text" + CSI + "0m")


class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.grid = np.array([list(line) for line in lines])
        logging.info(self.grid)
        # find starting point
        all_coords = np.where(self.grid == 'S')
        # np.where returns a tuple (one element for each dimension); we know we have exactly one S element, therefore, it's safe to extract it
        self.start_coords = (all_coords[0][0], all_coords[1][0])
        logging.info(f"start coords: {self.start_coords}")

        #logging.info(self.lines)
        # build graph
        """        G = nx.Graph()
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
        """

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    # find coordinates for next step
    # we also provide the previous coordinates to avoid visiting the cell again from which we came in the previous step
    def find_next_coords(self, prev_coords, curr_coords):
        ch = self.grid[curr_coords[0], curr_coords[1]]
        res = None
        if ch == '|': # | is a vertical pipe connecting north and south
            if prev_coords[0] == curr_coords[0]-1: # we came from north -> go south
                res = (curr_coords[0]+1, curr_coords[1])
            else: # we came from south -> go north
                res = (curr_coords[0]-1, curr_coords[1])
        elif ch == '-': # - is a horizontal pipe connecting east and west
            if prev_coords[1] == curr_coords[1]-1: # we came from west -> go east
                res = (curr_coords[0], curr_coords[1]+1)
            else: # we came from east -> go west
                res = (curr_coords[0], curr_coords[1]-1)
        elif ch == 'L': # L is a 90-degree bend connecting north and east
            if prev_coords[1] == curr_coords[1]+1: # we came from east -> go north
                res = (curr_coords[0]-1, curr_coords[1])
            else: # we came from north -> go east
                res = (curr_coords[0], curr_coords[1]+1)
        elif ch == 'J': # J is a 90-degree bend connecting north and west
            if prev_coords[1] == curr_coords[1]-1: # we came from west -> go north
                res = (curr_coords[0]-1, curr_coords[1])
            else: # we came from north -> go west
                res = (curr_coords[0], curr_coords[1]-1)
        elif ch == '7': # 7 is a 90-degree bend connecting south and west
            if prev_coords[1] == curr_coords[1]-1: # we came from west -> go south
                res = (curr_coords[0]+1, curr_coords[1])
            else: # we came from south -> go west
                res = (curr_coords[0], curr_coords[1]-1)
        elif ch == 'F': # F is a 90-degree bend connecting south and east
            if prev_coords[1] == curr_coords[1]+1: # we came from east -> go south
                res = (curr_coords[0]+1, curr_coords[1])
            else: # we came from south -> go east
                res = (curr_coords[0], curr_coords[1]+1)
        elif ch == 'S': # S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has
            # can we walk east? 
            if curr_coords[1] < len(self.grid[0])-1:
                neighbour = (curr_coords[0], curr_coords[1]+1)
                ch_neighbour = self.grid[neighbour] 
                if ch_neighbour in ['-', 'J', '7']:
                    res = neighbour
            # can we walk west?
            elif curr_coords[1] > 0:
                neighbour = (curr_coords[0], curr_coords[1]-1)
                ch_neighbour = self.grid[neighbour] 
                if ch_neighbour in ['-', 'L', 'F']:
                    res = neighbour
            # can we walk north?
            elif curr_coords[0] > 0:
                neighbour = (curr_coords[0]-1, curr_coords[1])
                ch_neighbour = self.grid[neighbour] 
                if ch_neighbour in ['|', '7', 'F']:
                    res = neighbour
            else:
                raise Exception(f"no valid path found from starting point")
        else:
            raise Exception(f"invalid character at position {curr_coords}: {ch}")
        logging.info(f"walk: prev = {prev_coords}, curr = {curr_coords} / {ch}, next = {res}")
        return res


    def find_loop(self):
        path = [self.start_coords]
        # starting from starting point S, find the direction in which we need to go 
        curr_coords = self.start_coords
        next_coords = self.find_next_coords(None, curr_coords)
        while next_coords != self.start_coords:
            path.append(next_coords)
            prev_coords = curr_coords
            curr_coords = next_coords
            next_coords = self.find_next_coords(prev_coords, curr_coords)
        logging.info(f"found path: {path} with length: {len(path)}")
        return path

    def solve_part_I(self):
        path = self.find_loop()
        return len(path) // 2

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


