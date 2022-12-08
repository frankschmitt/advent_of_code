import unittest
from TreetopTreeHouse import TreetopTreeHouse

class TreetopTreeHouse_Test(unittest.TestCase):

    def test_solve_part_I(self):
        tth = TreetopTreeHouse.read_input_file('example_input.txt')
        self.assertEqual(21, tth.solve_part_I())

if __name__ == '__main__':
    unittest.main()
