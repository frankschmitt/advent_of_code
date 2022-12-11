# CathodRayTubes_Test.py
import unittest
from CathodRayTubes import CathodRayTubes

class CathodRayTubesTest(unittest.TestCase):

    def test_simple_example(self):
        pass
        crt = CathodRayTubes.read_input_file('example_input-small.txt')
        crt.solve_part_I()
        got = [crt.memory[i]['x'] for i in [1, 2, 3, 4, 5] ]
        self.assertEqual([1, 1, 1, 4, -1], got) 

    def test_solve_part_I(self):
        crt = CathodRayTubes.read_input_file('example_input.txt')
        self.assertEqual(13140, crt.solve_part_I())

if __name__ == '__main__':
    unittest.main()


# 1 + 15 - 11 + 6 - 3 + 5 - 1 - 8 + 13 + 4 = 21.)
