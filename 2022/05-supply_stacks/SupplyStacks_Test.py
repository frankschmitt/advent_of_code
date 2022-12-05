import unittest
import SupplyStacks as SS

class SupplyStacksTest(unittest.TestCase):

    def test_parse_instruction(self):
        self.assertEqual({'n': 1, 'source': 2, 'target': 3}, SS.parse_instruction("move 1 from 2 to 3"))
        self.assertEqual({'n': 99, 'source': 17, 'target': 105}, SS.parse_instruction("move 99 from 17 to 105\n"))

    def test_parse_crate_line(self):
        self.assertEqual({1: [], 2: ['D']}, SS.parse_crate_line('    [D]'))
        self.assertEqual({1: ['N'], 2: ['C'], 3: []}, SS.parse_crate_line('[N] [C]    '))

    def test_read_example_input(self):
        ss = SS.SupplyStacks.read_input_file('example_input.txt')
        self.assertEqual({1: ['Z', 'N'], 2: ['M', 'C', 'D'], 3: ['P']}, ss.stacks)

if __name__ == '__main__':
    unittest.main()

