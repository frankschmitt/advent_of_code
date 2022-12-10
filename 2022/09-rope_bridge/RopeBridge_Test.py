import unittest
from RopeBridge import RopeBridge

class RopeBridgeTest(unittest.TestCase):

    def test_solve_part_I(self):
        rb = RopeBridge.read_input_file('example_input.txt')
        self.assertEqual(13, rb.solve_part_I())

if __name__ == '__main__':
    unittest.main()
