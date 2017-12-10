package spiral_memory

import (
    "fmt"
    "testing"
)

func ExpectEqual(t *testing.T, expected uint64, actual uint64) {
    // Having to do this manually is incredibly stupid. Does Go really not have ExpectEquals() or similar?
    if expected != actual {
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

func TestSumInSpiralMemoryForSampleTestCases(t *testing.T) {
    ExpectEqual(t, 1, SumInSpiralMemory(1))
    ExpectEqual(t, 1, SumInSpiralMemory(2))
    ExpectEqual(t, 2, SumInSpiralMemory(3))
    ExpectEqual(t, 4, SumInSpiralMemory(4))
    ExpectEqual(t, 5, SumInSpiralMemory(5))
    ExpectEqual(t, 10, SumInSpiralMemory(6))
    ExpectEqual(t, 806, SumInSpiralMemory(23))
}

func TestSumInSpiralMemorySolution(t *testing.T) {
    ExpectEqual(t, 349975, FirstSumInSpiralMemoryGreaterThanThreshold(347991)) 
}

