import unittest
import RockPaperScissors as RPS

class RockPaperScissorsTest(unittest.TestCase):

  def test_solve_part_I(self):
    rps = RPS.RockPaperScissors.read_input_file('example_input.txt')
    self.assertEqual(15, rps.solve_part_I())  

  def test_solve_part_I(self):
    rps = RPS.RockPaperScissors.read_input_file('example_input.txt')
    self.assertEqual(12, rps.solve_part_II())  

if __name__ == '__main__':
    unittest.main()
