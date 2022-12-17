import unittest
import logging
from DistressSignal import DistressSignal, Pair

class DistressSignalTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  

    def test_solve_part_I(self):
        ds = DistressSignal.read_input_file('example_input.txt')
        self.assertEqual(13, ds.solve_part_I())

    def test_is_in_right_order(self):
        p1 = Pair(1, "[1,1,3,1,1]", "[1,1,5,1,1]")
        self.assertEqual(p1.is_in_right_order(), True)
        p2 = Pair(2, "[[1],[2,3,4]]", "[[1],4]")
        self.assertEqual(p2.is_in_right_order(), True)
        p3 = Pair(3, "[9]", "[[8,7,6]]")
        self.assertEqual(p3.is_in_right_order(), False)
        p4 = Pair(4, "[[4,4],4,4]", "[[4,4],4,4,4]")
        self.assertEqual(p4.is_in_right_order(), True)
        p5 = Pair(5, "[7,7,7,7]", "[7,7,7]")
        self.assertEqual(p5.is_in_right_order(), False)



if __name__ == '__main__':
    unittest.main()

