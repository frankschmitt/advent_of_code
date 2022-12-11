# MonkeyInTheMiddle_Test.py
import unittest
from MonkeyInTheMiddle import MonkeyInTheMiddle, Monkey

class MonkeyInTheMiddle_Test(unittest.TestCase):

    def test_read_monkeys(self):
        mitm = MonkeyInTheMiddle.read_input_file('example_input.txt')
        self.assertEqual(4, len(mitm.monkeys))
        # check first monkey
        monkey0 = mitm.monkeys[0]
        self.assertEqual(0, monkey0.index) 
        self.assertEqual([79, 98], monkey0.items)
        self.assertEqual(('*', '19'), monkey0.operation)
        self.assertEqual(23, monkey0.test)
        self.assertEqual(2, monkey0.true_action)
        self.assertEqual(3, monkey0.false_action)
        # check last monkey
        monkey3 = mitm.monkeys[3]
        self.assertEqual(3, monkey3.index) 
        self.assertEqual([74], monkey3.items)
        self.assertEqual(('+', '3'), monkey3.operation)
        self.assertEqual(17, monkey3.test)
        self.assertEqual(0, monkey3.true_action)
        self.assertEqual(1, monkey3.false_action)

    def test_solve_part_I(self):
        mitm = MonkeyInTheMiddle.read_input_file('example_input.txt')
        self.assertEqual(10605, mitm.solve_part_I())
   

if __name__ == '__main__':
    unittest.main()
