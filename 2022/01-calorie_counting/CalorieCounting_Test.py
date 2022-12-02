import unittest

import CalorieCounting as CC

class CalorieCountingTest(unittest.TestCase):

    def test_read_example_input(self):
        cc = CC.CalorieCounting.read_input_file('example_input.txt')
        self.assertEqual(5, len(cc.elves), '#elves')
        self.assertEqual([1000, 2000, 3000], cc.elves[0].calories, 'calories for first elf')

if __name__ == '__main__':
    unittest.main()
