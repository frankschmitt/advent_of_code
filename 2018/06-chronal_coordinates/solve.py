import string

# read an input file, parse it, and return a list of (x,y) coordinates 
def read_input(filename):
  coords = {} 
  with open(filename, "r") as input:
    lines = input.readlines()
    for i in range(len(lines)):
      a = string.split(lines[i], ',')
      coords[i] = (int(a[0]), int(a[1]))
  return coords

# extract the x coordinates from a list of (x,y) coordinate pairs
def get_x(coords):
  result = []
  for v in coords.values(): 
    result.append(v[0])
  return result

# extract the y coordinates from a list of (x,y) coordinate pairs
def get_y(coords):
  result = []
  for v in coords.values(): 
    result.append(v[1])
  return result

# compute the Manhattan (aka City Block) distance between two locations
def manhattan_distance(c1, c2):
  return abs(c1[0] - c2[0]) + abs(c1[1] - c2[1])

# given a list of locations and a coordinate, find the nearest location 
def find_nearest_location(coords, (x, y)):
  best = []
  best_dist = 1e6
  for (k,v) in coords.items():
    dist = manhattan_distance((x,y), (v[0], v[1]))
    if (len(best) == 0) or (dist < best_dist):
      best_dist = dist
      best = [k]
    else:
      if dist == best_dist: # tie
        best.append(k)
  if len(best) == 1:
    return best[0]
  else:
    return -1 

# given a list of coordinates (x,y), build the grid
def build_grid(coords):
  max_x = max(get_x(coords)) + 1
  max_y = max(get_y(coords)) + 1 
  grid = []
  # initialize the grid with -1 (unoccupied)
  for rowidx in range(max_y):
    row = [] 
    for colidx in range(max_x):
      row.append(-1)
    grid.append(row)
  # for each square in the grid: find the nearest location, and mark the square
  for rowidx in range(max_y):
    for colidx in range(max_x):
      grid[rowidx][colidx] = find_nearest_location(coords, (colidx, rowidx))
  return grid

# check whether a given index occupies at least one border square
def occupies_border_square(idx, grid):
  last_row = len(grid) - 1
  last_col = len(grid[0]) - 1
  # check first and last row
  for i in range(0,last_col+1): 
    if (grid[0][i] == idx): 
      return True
    if (grid[last_row][i] == idx): 
      return True
  # check first and last column
  for i in range(0, last_row + 1):
    if (grid[i][0] == idx): 
      return True
    if (grid[i][last_col] == idx):
      return True
  return False

# given the grid and list of coords, return the coords with finite area
def find_candidate_coords(grid, coords):
  result = {}
  for (k,v) in coords.items():
    if not occupies_border_square(k, grid):
      result[k] = v
  return result

# get the area occupied by a idx
def get_occupied_area(grid, idx):
  area = 0
  for rowidx in range(len(grid)):
    for colidx in range(len(grid[rowidx])):
      if grid[rowidx][colidx] == idx:
        area += 1
  return area

# given the grid and list of coords, return the largest occupied area
def find_largest_area(grid, coords):
  largest = 0
  for (k,v) in coords.items():
    area = get_occupied_area(grid, k)
    if area > largest:
      largest = area
  return largest

# print routine, works only for sample grid
def print_grid(grid):
  s = "   012345678"
  print(s)
  for rowidx in range(len(grid)):
    s = str(rowidx) + ": "
    for colidx in range(len(grid[rowidx])):
      if grid[rowidx][colidx] == -1:
        s += "."
      else:
        s += str(grid[rowidx][colidx])
    print(s)

def solve_partI(filename):
  coords = read_input(filename)
  grid = build_grid(coords)
  candidate_coords = find_candidate_coords(grid, coords)
  area = find_largest_area(grid, candidate_coords)
  return area

def is_safe_square(coords, (x,y), threshold):
  sum = 0
  for (k,v) in coords.items():
    dist = manhattan_distance((x,y), (v[0], v[1]))
    sum += dist
    if sum > threshold:
      return False
  return (sum < threshold)

def find_safe_squares(coords, overall_threshold):
  # the sum of manhattan distances from n points is at least n * min(distance)
  #   therefore, we only consider points within overall_treshold / n distance from the outermost points
  threshold = overall_threshold / len(coords) + 1
  min_x = -threshold
  min_y = -threshold
  max_x = max(get_x(coords)) + threshold 
  max_y = max(get_y(coords)) + threshold 
  result = []
  for x in range(min_x, max_x + 1):
    for y in range(min_y, max_y + 1):
      if is_safe_square(coords, (x,y), overall_threshold):
        result.append((x,y,))
  return result

def solve_partII(filename, overall_threshold):
  coords = read_input(filename)
  grid = build_grid(coords)
  safe_squares = find_safe_squares(coords, overall_threshold)
  return len(safe_squares) 

#print(solve_partI("sample_input.txt"))
print(solve_partI("input.txt"))
#print(solve_partII("sample_input.txt", 32))
print(solve_partII("input.txt", 10000))

