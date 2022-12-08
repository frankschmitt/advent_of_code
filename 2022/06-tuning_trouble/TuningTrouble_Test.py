import unittest
import TuningTrouble as TT

class TuningTroubleTest(unittest.TestCase):

    def test_solve_part_I(self):
        tt = TT.TuningTrouble.read_input_file('example_input.txt')
        self.assertEqual(7, tt.solve_part_I())

if __name__ == '__main__':
    unittest.main()
