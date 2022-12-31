import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO)
 
    def test_read_input(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual('.', solve.grid[0,0])
        self.assertEqual('#', solve.grid[503,4])
        self.assertEqual('#', solve.grid[502,4])
        self.assertEqual('.', solve.grid[501,4])
        self.assertEqual('#', solve.grid[494,9])
        self.assertEqual(9, solve.bottom)
        print(solve)

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(24, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(93, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

