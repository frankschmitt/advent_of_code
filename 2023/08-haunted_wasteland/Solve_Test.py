import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO)
  

    def test_solve_part_I_short_path(self):
        #solve = Solve.read_input_file('example_input_1.txt')
        #self.assertEqual(2, solve.solve_part_I())
        pass

    def test_solve_part_I_long_path(self):
        #solve = Solve.read_input_file('example_input_2.txt')
        #self.assertEqual(6, solve.solve_part_I())
        pass

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input_3.txt')
        self.assertEqual(6, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

