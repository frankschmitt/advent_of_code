from pipe import Pipe, map, sort

class Elf:
  calories = []
 
  def __init__(self, calories = []):
      self.calories = calories
      self.sum_calories = sum(self.calories)

@Pipe
def split_elves(iterable):
  e = []
  for line in iterable:
      if line == "\n":
          yield Elf(e)
          e = []
      else:
          e.append(int(line))
  yield Elf(e)

 
class CalorieCounting:
  elves = []

  def __init__(self, elves = []):
      self.elves = elves

  def read_input_file(filename):
    with open(filename) as f:
      lines = f.readlines()
    elves = list(lines
                 | split_elves
                )
      
    cc = CalorieCounting(elves)
    return cc

  def get_top_n_scores(n):
    sorted_scores = list(self.elves
                         | map(lambda e: e.sum_calories)
                         | sort(reverse = True)
                         )

  def solve_part_I(self):
    sorted_scores = list(self.elves
                         | map(lambda e: e.sum_calories)
                         | sort(reverse = True)
                         )
    return sorted_scores[0]

if __name__ == '__main__':
    cc = CalorieCounting.read_input_file('input.txt')
    print("{}".format(cc.solve_part_I()))
