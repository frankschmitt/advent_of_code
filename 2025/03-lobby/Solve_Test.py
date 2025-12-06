import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(357, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(3121910778619, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

