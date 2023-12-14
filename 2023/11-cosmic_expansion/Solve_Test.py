import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  
    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(374, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(374, solve.solve_part_II(2))
        self.assertEqual(1030, solve.solve_part_II(10))
        self.assertEqual(8410, solve.solve_part_II(100))

if __name__ == '__main__':
    unittest.main()

