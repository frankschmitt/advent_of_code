import unittest
import logging
from Solve import Solve, Game, Set

class SolveTest(unittest.TestCase):
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.DEBUG)
  
    def test_parse_line(self):
        g = Game.parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") 
        self.assertEqual(str(g), str(Game(1,[Set(4,0,3), Set(1,2,6), Set(0,2,0)])))

    def test_solve_part_I(self):
        solve = Solve.read_input_file('example_input.txt')
        self.assertEqual(8, solve.solve_part_I())
        #pass

    def test_solve_part_II(self):
        #solve = Solve.read_input_file('example_input.txt')
        #self.assertEqual(-3, solve.solve_part_II())
        pass

if __name__ == '__main__':
    unittest.main()

