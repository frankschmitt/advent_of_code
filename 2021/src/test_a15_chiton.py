import unittest
from a15_chiton import Chiton

class ChitonTestCase(unittest.TestCase):

   def test_expand_grid(self):
       result = Chiton.expand_grid([[8]])
       self.assertEqual(result, [[8, 9, 1, 2, 3],
                                 [9, 1, 2, 3, 4],
                                 [1, 2, 3, 4, 5],
                                 [2, 3, 4, 5, 6],
                                 [3, 4, 5, 6, 7]
                                ]
      )
