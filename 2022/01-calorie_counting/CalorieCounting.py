from pipe import Pipe

class Elf:
  calories = []
 
  def __init__(self, calories = []):
      self.calories = calories

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
