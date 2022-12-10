import unittest
from NoSpaceLeftOnDevice import NoSpaceLeftOnDevice

class NoSpaceLeftOnDeviceTest(unittest.TestCase):

    def test_rec_size_simple(self):
        ns = NoSpaceLeftOnDevice.read_input_file('example_input.txt')
        self.assertEqual(584, ns.rec_size('/a/e'))

    def test_rec_size_with_children(self):
        ns = NoSpaceLeftOnDevice.read_input_file('example_input.txt')
        self.assertEqual(94853, ns.rec_size('/a'))

    def test_solve_part_I(self):
        ns = NoSpaceLeftOnDevice.read_input_file('example_input.txt')
        self.assertEqual(95437, ns.solve_part_I())

    def test_solve_part_II(self):
        ns = NoSpaceLeftOnDevice.read_input_file('example_input.txt')
        self.assertEqual(24933642, ns.solve_part_II())


if __name__ == '__main__':
    unittest.main()
