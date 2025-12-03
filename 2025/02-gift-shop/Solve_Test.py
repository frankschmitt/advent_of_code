import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)

    def test_is_invalid(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(True, solve.is_invalid(99))
        self.assertEqual(True, solve.is_invalid(1212))
        self.assertEqual(False, solve.is_invalid(12312))

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(1227775554, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(-3, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

