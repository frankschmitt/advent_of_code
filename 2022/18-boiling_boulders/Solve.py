import logging
import functools
import numpy as np

# e.g. cube = (1,1,1) => faces are:
#   (0,0,0), (0,1,0), (0,1,1), (0,0,1) # yz plane ("left")
#   (1,0,0), (1,1,0), (1,1,1), (1,0,1) # yz plane ("right")
#   (0,0,0), (1,0,0), (1,0,1), (0,0,1) # xz plane ("back")
#   (0,1,0), (1,1,0), (1,1,1), (0,1,1) # xz plane ("front")
#   (0,0,0), (1,0,0), (1,1,0), (0,1,0) # xy plane ("bottom")
#   (0,0,1), (1,0,1), (1,1,1), (0,1,1) # xy plane ("top")
class Cube:
    def __init__(self, coords):
        self.x, self.y, self.z = coords[0], coords[1], coords[2]
        x, y, z = self.x, self.y, self.z
        # we store each face in the format [p1, p2, p3, p4] and each p in format (x,y,z)
        left  = [(x-1,y-1,z-1), (x-1,y,z-1), (x-1,y,z), (x-1,y-1,z)]  
        right = [(x,y-1,z-1),   (x,y,z-1),   (x,y,z),   (x,y-1,z)]  
        back  = [(x-1,y-1,z-1), (x,y-1,z-1), (x,y-1,z), (x-1,y-1,z)] 
        front = [(x-1,y,z-1),   (x,y,z-1),   (x,y,z),   (x-1,y,z)] 
        bottom= [(x-1,y-1,z-1), (x,y-1,z-1), (x,y,z-1), (x-1,y,z-1)]
        top   = [(x-1,y-1,z),   (x,y-1,z),   (x,y,z),   (x-1,y,z)]
        self.faces = [left, right, back, front, bottom, top]

class Solve:
    def __init__(self, lines):
        self.lines = lines
        self.cubes= [Cube(np.fromstring(s, dtype=int, sep=',')) for s in lines]
        # build list of faces (6 per cube)
        self.faces = [f for c in self.cubes for f in c.faces]

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    """Idea:
        - all_sides = [] (or empty dict)
        - for each cube: store all its sides in all_sides
        - remove duplicates from all_sides (those are the sides that are shared between two cubes)
        - return len(all_sides)
    """
    def solve_part_I(self):
        unique_faces = {}
        for f in self.faces:
            if str(f) in unique_faces:
                del unique_faces[str(f)]
            else:
                unique_faces[str(f)] = True # dummy value
        return len(unique_faces)

    def solve_part_II(self):
        return -1

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    solve = Solve.read_input_file('input.txt')
    print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))


