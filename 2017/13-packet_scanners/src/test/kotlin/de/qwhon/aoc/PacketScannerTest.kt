package de.qwhon.aoc

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test

class PacketScannerTest {

    @Test
    fun testParsingSampleInputShouldReturnCorrectHashMap() {
        assertEquals("parse sample input", hashMapOf(0 to 3, 1 to 2, 4 to 4, 6 to 4), PacketScanner.parseInputFile("sample_input.txt"))
    }

    @Test
    fun testSolveShouldReturnCorrectSolutionForExample() {
        assertEquals("severity for sample input: 0*3 + 6*4 = 24", 24, PacketScanner.severityOfViolationsForInputFile("sample_input.txt"))
    }

    @Test
    fun testSolveShouldSolvePartI() {
        assertEquals("severity for puzzle input", 1876, PacketScanner.severityOfViolationsForInputFile("input.txt"))

    }

    // Tests for minimum delay
    @Test
    fun testMinimumDelayShouldReturn10ForSampleInput() {
        assertEquals("minimum delay for sample input should be 10",
                10,
                PacketScanner.minimumDelayForInputFile("sample_input.txt"))
    }

    // with the right approach, ridiculously fast: 415 ms (whereas checking numbers up to 150k took 3min with the previous approach
    @Test
    fun testMinimumDelayShouldSolvePartII() {
        assertEquals("minimum delay for puzzle input",
                3964778,
                PacketScanner.minimumDelayForInputFile("input.txt"))
    }

}

/*
  depth: 0, range: 3
  depth: 1, range: 2
  depth: 4, range: 4
  depth: 6, range: 4
  0:       0121012101210
  1:      0101010101010
  4:   0123210123210
  6: 0123454321012
 */