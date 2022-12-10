import unittest
from RopeBridge import RopeBridge

class RopeBridgeTest(unittest.TestCase):

    def test_solve_part_I(self):
        rb = RopeBridge.read_input_file('example_input.txt')
        self.assertEqual(13, rb.solve_part_I())

    def test_solve_part_II_small(self):
        rb = RopeBridge.read_input_file('example_input.txt')
        self.assertEqual(1, rb.solve_part_II())

    def test_solve_part_II_medium(self):
        rb = RopeBridge.read_input_file('example_input_medium.txt')
        self.assertEqual(36, rb.solve_part_II())

if __name__ == '__main__':
    unittest.main()
