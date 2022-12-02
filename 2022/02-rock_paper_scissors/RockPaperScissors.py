from pipe import map

class RockPaperScissors:
  score_dict1 = {
    # A rock / B paper / C scissors vs X rock / Y paper / Z scissors
    # base scores: 1 for rock, 2 for paper, 3 for scissors
    # round score: 6 for win, 3 for draw, 0 for loss
    'A X': 4, # Rock vs Rock, 1 base + 3 for draw 
    'A Y': 8, # Rock vs Paper, 2 base + 6 for win
    'A Z': 3, # Rock vs Scissors, 3 base + 0 for loss
    'B X': 1, # Paper vs Rock, 1 base + 0 for loss
    'B Y': 5, # Paper vs Paper, 2 base + 3 for draw
    'B Z': 9, # Paper vs Scissors, 3 base + 6 for win 
    'C X': 7, # Scissors vs Rock, 1 base + 6 for win
    'C Y': 2, # Scissors vs Paper, 2 base + 0 for loss 
    'C Z': 6, # Scissors vs Scissors, 3 base + 3 for draw
  }

  score_dict2 = {
    # A rock / B paper / C scissors, X lose, Y draw, Z win
    # base scores: 1 for rock, 2 for paper, 3 for scissors
    # round score: 6 for win, 3 for draw, 0 for loss
    'A X': 3, # Rock vs Scissors, 3 base + 0 for loss 
    'A Y': 4, # Rock vs Rock, 1 base + 3 for draw
    'A Z': 8, # Rock vs Paper, 2 base + 6 for win
    'B X': 1, # Paper vs Rock, 1 base + 0 for loss
    'B Y': 5, # Paper vs Paper, 2 base + 3 for draw
    'B Z': 9, # Paper vs Scissors, 3 base + 6 for win 
    'C X': 2, # Scissors vs Paper, 2 base + 0 for loss
    'C Y': 6, # Scissors vs Scissors, 3 base + 3 for draw
    'C Z': 7, # Scissors vs Rock, 1 base + 6 for win
  }

  def __init__(self, lines):
    self.score1 = sum(lines
                      | map(lambda x: x.rstrip())
                      | map(lambda x: self.score_dict1[x])
                     )
    self.score2 = sum(lines
                      | map(lambda x: x.rstrip())
                      | map(lambda x: self.score_dict2[x])
                     )

  def solve_part_I(self):
    return self.score1

  def solve_part_II(self):
    return self.score2

  def read_input_file(filename):
    with open(filename) as f:
      lines = f.readlines()
    return RockPaperScissors(lines)

if __name__ == '__main__':
  rps = RockPaperScissors.read_input_file('input.txt')
  print("{} {}".format(rps.solve_part_I(), rps.solve_part_II()))

