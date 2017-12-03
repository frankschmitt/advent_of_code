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
   ExpectEqual(t, 1, 2)
}


