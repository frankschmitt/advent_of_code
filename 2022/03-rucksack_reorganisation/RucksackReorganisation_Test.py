import unittest
import RucksackReorganisation as RR

class RucksackReorganisationTest(unittest.TestCase):

    def test_solve_part_I(self):
        rr = RR.RucksackReorganisation.read_input_file('example_input.txt')
        self.assertEqual(157, rr.solve_part_I())

if __name__ == '__main__':
    unittest.main()
