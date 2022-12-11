# CathodRayTubes_Test.py
import unittest
from CathodRayTubes import CathodRayTubes

class CathodRayTubesTest(unittest.TestCase):

    def test_simple_example(self):
        pass
        crt = CathodRayTubes.read_input_file('example_input-small.txt')
        crt.solve_it()
        got = [crt.memory[i] for i in [1, 2, 3, 4, 5, 6] ]
        self.assertEqual([1, 1, 1, 4, 4, -1], got) 

    def test_solve_it(self):
        crt = CathodRayTubes.read_input_file('example_input.txt')
        self.assertEqual(13140, crt.solve_it())

if __name__ == '__main__':
    unittest.main()
