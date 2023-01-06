import unittest
import logging
from Solve import Solve

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO)
  
    def test_parse_input(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(15, len(solve.monkeys))
        # m0: root: pppw + sjmn
        m0 = solve.monkeys["root"]
        self.assertEqual("root", m0.name)
        self.assertEqual("pppw", m0.lhs)
        self.assertEqual("+", m0.op)
        self.assertEqual("sjmn", m0.rhs)
        self.assertEqual(None, m0.value)
        # m1: dbpl: 5
        m1 = solve.monkeys["dbpl"]
        self.assertEqual("dbpl", m1.name)
        self.assertEqual(None, m1.lhs)
        self.assertEqual(None,  m1.op)
        self.assertEqual(None, m1.rhs)
        self.assertEqual(5, m1.value)

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(152, solve.solve_part_I())

    def test_solve_part_II(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(301, solve.solve_part_II())

if __name__ == '__main__':
    unittest.main()

