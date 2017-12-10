package spiral_memory

import (
	"math"
)

var DIM = 2000
var OFFSET = DIM / 2

type square struct {
	index    uint64
	distance uint64
	weight   uint64
}

type spiral []square
type matrix []uint64

func getIndex(x int, y int) uint64 {
	return uint64((OFFSET+x)*DIM + (OFFSET + y))
}

func getVal(mat matrix, x int, y int) uint64 {
	return mat[getIndex(x, y)]
}

func setVal(mat matrix, x int, y int, val uint64) matrix {
	mat[getIndex(x, y)] = val
	return mat
}

func computeWeight(mat matrix, x int, y int) uint64 {
	// root square? just return 1
	if x == 0 && y == 0 {
		return 1
	} else {
		var sum uint64 = 0
		sum += getVal(mat, x+1, y)   // right
		sum += getVal(mat, x+1, y+1) // top right
		sum += getVal(mat, x, y+1)   // top
		sum += getVal(mat, x-1, y+1) // top left
		sum += getVal(mat, x-1, y)   // left
		sum += getVal(mat, x-1, y-1) // bottom left
		sum += getVal(mat, x, y-1)   // bottom
		sum += getVal(mat, x+1, y-1) // bottom right
		return sum
	}
}

// solution taken from https://stackoverflow.com/a/398302/610979 and adapted to Go
// constructs a Spiral that's large enough to contain val (actually, it's a little
// bit larger, since we always construct a full nxn Spiral)
func Spiral(val uint64) []square {
	var dim = uint64(math.Ceil(math.Sqrt(float64(val))))
	var res = make([]square, dim*dim+1)
	// 2D matrix to keep track of sums in squares
	var sums = make([]uint64, DIM*DIM) // arbitrary: hopefully, 4M entries will suffice
	var x = 0
	var y = 0
	var dx = 0
	var dy = -1
	for i := uint64(1); i <= val; i++ {
		var weight = computeWeight(sums, x, y)
		setVal(sums, x, y, weight)
		res[i] = square{i,
			uint64(math.Abs(float64(x)) + math.Abs(float64(y))),
			weight}
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
	return spiral[square].distance
}

// compute the sum of adjacent squares up to the given cell
func SumInSpiralMemory(square uint64) uint64 {
	var spiral = Spiral(square)
	return spiral[square].weight
}
