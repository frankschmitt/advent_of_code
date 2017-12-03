package spiral_memory

import (
  "math"
)

// solution taken from https://stackoverflow.com/a/398302/610979 and adapted to Go
// constructs a Spiral that's large enough to contain val (actually, it's a little
// bit larger, since we always construct a full nxn Spiral)
func Spiral(val uint64) []uint64 {
  var dim = uint64(math.Ceil(math.Sqrt(float64(val))))
  var res = make([]uint64, dim*dim+1)
  var x = 0
  var y = 0
  var dx = 0
  var dy = -1
  for i := uint64(1); i <= val; i++ {
        //if ((-x/2 < x) && (x <= x/2) && (-y/2 < y) && (y <= y/2)) {
          res[i] = uint64(math.Abs(float64(x)) + math.Abs(float64(y)))
        //} 
        if (x == y) || ((x < 0) && (x == -y)) || ((x > 0) && (x == 1-y)) {
            dx, dy = -dy, dx
        }
        x, y = x+dx, y+dy
  }
  return res
}

func PrintSpiral(spiral []uint64) {
  for i := 0; i < len(spiral); i++ {
    print(spiral[i])
  } 
}

func DistanceInSpiralMemory(square uint64) uint64 {
  var spiral = Spiral(square)
  //PrintSpiral(spiral)
  return spiral[square]
}


