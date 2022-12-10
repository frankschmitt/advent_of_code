import unittest
from NoSpaceLeftOnDevice import NoSpaceLeftOnDevice

class NoSpaceLeftOnDeviceTest(unittest.TestCase):
    def test_solve_part_I(self):
        ns = NoSpaceLeftOnDevice.read_input_file('example_input.txt')
        self.assertEqual(95437, ns.solve_part_I())


if __name__ == '__main__':
    unittest.main()
