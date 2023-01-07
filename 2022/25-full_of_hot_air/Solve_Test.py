import unittest
import logging
from Solve import Solve, snafu_to_decimal, decimal_to_snafu, to_base_5

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)

    def xtest_to_base_5(self):
        self.assertEqual("4", to_base_5(4))
        self.assertEqual("10", to_base_5(5))
        self.assertEqual("14", to_base_5(9))
        self.assertEqual("21", to_base_5(11))
        self.assertEqual("44", to_base_5(24))
        self.assertEqual("100", to_base_5(25))
 
    def xtest_snafu_to_decimal(self):
        self.assertEqual(0, snafu_to_decimal("0"))
        self.assertEqual(11, snafu_to_decimal("21"))
        self.assertEqual(314159265, snafu_to_decimal("1121-1110-1=0"))

    def test_decimal_to_snafu(self):
        self.assertEqual("0", decimal_to_snafu(0))
        self.assertEqual("1=", decimal_to_snafu(3))
        self.assertEqual("1-", decimal_to_snafu(4))
        self.assertEqual("10", decimal_to_snafu(5))
        self.assertEqual("12", decimal_to_snafu(7))
        self.assertEqual("2=", decimal_to_snafu(8))
        self.assertEqual("2-", decimal_to_snafu(9))
        self.assertEqual("1=0", decimal_to_snafu(15))
        self.assertEqual(18, snafu_to_decimal(decimal_to_snafu(18)))
        self.assertEqual("1-0---0", decimal_to_snafu(12345))
        self.assertEqual(33038276688955, snafu_to_decimal(decimal_to_snafu(33038276688955)))
             # "20=2==1101==2=022=10"))

    def xtest_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual("2=-1=0", solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        #self.assertEqual(-3, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

