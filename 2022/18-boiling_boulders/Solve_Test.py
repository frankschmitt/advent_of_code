import unittest
import logging
import numpy as np
from Solve import Solve, Cube

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  

    def test_cube(self):
        c = Cube([1,1,1])
        self.assertEqual(6, len(c.faces))
        self.assertEqual([(0,0,0), (0,1,0), (0,1,1), (0,0,1)], c.faces[0])

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(13, len(solve.cubes))
        # check first cube
        c0 = solve.cubes[0]
        self.assertEqual((2, 2, 2), (c0.x, c0.y, c0.z))
        self.assertEqual(13*6, len(solve.faces))
        self.assertEqual(64, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(-3, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

