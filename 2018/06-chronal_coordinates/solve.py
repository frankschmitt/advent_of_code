import string

def read_input(filename):
  coords = []
  with open(filename, "r") as input:
    lines = input.readlines()
    for i in range(len(lines)):
      a = string.split(lines[i], ',')
      #print(lines[i])
      #print(a)
      coords.append( (int(a[0]), int(a[1])))
  return coords

def get_x(coords):
  result = []
  for i in range(len(coords)):
    result.append(coords[i][0])
  return result

def get_y(coords):
  result = []
  for i in range(len(coords)):
    result.append(coords[i][1])
  return result

def build_grid(coords):
  max_x = max(get_x(coords))
  max_y = max(get_y(coords))
  print(max_x, max_y)
  grid = []
  for rowidx in range(max_y):
    row = [] 
    for colidx in range(max_x):
      row.append('.')
    grid.append(row)
  return grid


  
coords = read_input("sample_input.txt")
grid = build_grid(coords)
print(grid)  
