import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)

    def test_is_invalid_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(True, solve.is_invalid_part_I(99))
        self.assertEqual(True, solve.is_invalid_part_I(1212))
        self.assertEqual(False, solve.is_invalid_part_I(12312))
        self.assertEqual(False, solve.is_invalid_part_I(999))

    def test_is_invalid_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(False, solve.is_invalid_part_II(12312))
        self.assertEqual(True, solve.is_invalid_part_II(99))
        self.assertEqual(True, solve.is_invalid_part_II(1212))
        self.assertEqual(True, solve.is_invalid_part_II(999))


    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(1227775554, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(4174379265, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

