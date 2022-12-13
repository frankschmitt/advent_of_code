import unittest
import logging
from HillClimbingAlgorithm import HillClimbingAlgorithm

class HillClimbingAlgorithmTest(unittest.TestCase):

    def test_solve_part_I(self):
        hca = HillClimbingAlgorithm.read_input_file('example_input.txt')
        self.assertEqual(31, hca.solve_part_I())

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    unittest.main()

