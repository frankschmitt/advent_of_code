# CampCleanup_Test.py
import unittest
import CampCleanup as CC

class CampCleanupTest(unittest.TestCase):

    def test_solve_part_I(self):
        cc = CC.CampCleanup.read_input_file('example_input.txt')
        self.assertEqual(2, cc.solve_part_I())

    def test_solve_part_I(self):
        cc = CC.CampCleanup.read_input_file('example_input.txt')
        self.assertEqual(4, cc.solve_part_II())

if __name__ == '__main__':
    unittest.main()
