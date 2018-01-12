package de.qwhon.aoc

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test

class PacketScannerTest {

    @Test
    fun testParsingSampleInputShouldReturnCorrectHashMap() {
        assertEquals("parse sample input",
                listOf(ScannerRecord(depth = 0, range = 3),
                        ScannerRecord(depth = 1, range = 2),
                        ScannerRecord(depth = 4, range = 4),
                        ScannerRecord(depth = 6, range = 4)),
                parseInputFile("sample_input.txt"))
    }

    @Test
    fun testSeverityOfViolationsShouldReturnCorrectSolutionForExample() {
        assertEquals("severity for sample input: 0*3 + 6*4 = 24", 24, severityOfViolationsForInputFile("sample_input.txt"))
    }

    @Test
    fun testSeverityOfViolationsShouldSolvePartI() {
        assertEquals("severity for puzzle input", 1876, severityOfViolationsForInputFile("input.txt"))

    }

    // Tests for minimum delay
    @Test
    fun testMinimumDelayShouldReturn10ForSampleInput() {
        assertEquals("minimum delay for sample input should be 10",
                10,
                minimumDelayForInputFile("sample_input.txt"))
    }

    // with the right approach, ridiculously fast: 415 ms (whereas checking numbers up to 150k took 3min with the previous approach
    @Test
    fun testMinimumDelayShouldSolvePartII() {
        assertEquals("minimum delay for puzzle input",
                3964778,
                minimumDelayForInputFile("input.txt"))
    }

}
