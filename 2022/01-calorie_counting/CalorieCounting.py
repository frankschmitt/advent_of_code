from pipe import Pipe, map, sort, take

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

  def sum_top_n_scores(self, n):
    return sum(self.elves
                | map(lambda e: e.sum_calories)
                | sort(reverse = True)
                | take(n)
                )

  def solve_part_I(self):
    return self.sum_top_n_scores(1)

  def solve_part_II(self):
    return self.sum_top_n_scores(3)

if __name__ == '__main__':
    cc = CalorieCounting.read_input_file('input.txt')
    print("{} {}".format(cc.solve_part_I(), cc.solve_part_II()))
