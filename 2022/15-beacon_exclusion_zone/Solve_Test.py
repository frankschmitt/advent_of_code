import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO)
  
    def test_parse_line(self):
        solve = Solve(["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"])
        self.assertEqual([[2, 18, -2, 15]], solve.beacons)

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(26, solve.solve_part_I(10))

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        #self.assertEqual(-3, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

