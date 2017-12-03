package spiral_memory

import (
  "testing"
  "fmt"
)

func ExpectEqual(t *testing.T, expected uint64, actual uint64) {
  // Having to do this manually is incredibly stupid. Does Go really not have ExpectEquals() or similar?
  if (expected != actual) {
    t.Error(fmt.Printf("Expected :%d, actual: %d\n", expected, actual))
  }
}

func TestDistanceInSpiralMemoryForSampleTestCases(t *testing.T) {
   ExpectEqual(t, 0, DistanceInSpiralMemory(1))  // 1 = origin - no walking required
   ExpectEqual(t, 3, DistanceInSpiralMemory(12)) // down, left, left
   ExpectEqual(t, 2, DistanceInSpiralMemory(23)) 
   ExpectEqual(t, 31, DistanceInSpiralMemory(1024)) 
}


func TestDistanceInSpiralMemorySolution(t *testing.T) {
   ExpectEqual(t, 480, DistanceInSpiralMemory(347991))  
}
