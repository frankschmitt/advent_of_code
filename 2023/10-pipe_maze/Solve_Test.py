import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  

    def test_solve_part_I_simple_loop(self):
        #solve = Solve.read_input_file('example_input_1.txt')
        #self.assertEqual(4, solve.solve_part_I())
        pass

    def test_solve_part_I_complex_loop(self):
        #solve = Solve.read_input_file('example_input_2.txt')
        #self.assertEqual(8, solve.solve_part_I())
        pass

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input_3.txt')
        self.assertEqual(10, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

