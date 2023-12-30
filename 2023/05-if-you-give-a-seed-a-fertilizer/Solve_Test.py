import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  

    def test_solve_part_I(self):
        #solve = Solve.read_input_file('example_input.txt')
        #self.assertEqual(35, solve.solve_part_I())
        pass

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(46, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

