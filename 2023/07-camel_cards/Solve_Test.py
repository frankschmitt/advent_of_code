import unittest
import logging
from Solve import Solve, Hand, HandType

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
 
    def test_compare(self):
        #c_ktjjt = Hand("KTJJT", 220)
        #c_kk677 = Hand("KK677", 28)
        #self.assertEqual(-1, Hand.compare(c_ktjjt, c_kk677))
        pass

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(6440, solve.solve_part_I())

    def test_solve_part_II(self):
        #solve = Solve.read_input_file('example_input.txt')
        #self.assertEqual(-3, solve.solve_part_II())
        pass

if __name__ == '__main__':
    unittest.main()

