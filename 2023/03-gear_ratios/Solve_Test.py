import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO)

    def xtest_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(4361, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(467835, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

