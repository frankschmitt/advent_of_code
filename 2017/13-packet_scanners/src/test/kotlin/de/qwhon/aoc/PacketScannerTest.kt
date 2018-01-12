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
    fun testGetScannerForSampleInputShouldBeCorrect() {
        val got = PacketScanner.getScannerForInputFile("sample_input.txt")
        assertEquals("scanner for sample input",
                PacketScanner(currentLayerOfPacket = -1,
                        layers = hashMapOf(0 to ScanningLayer(3),
                                1 to ScanningLayer(2),
                                4 to ScanningLayer(4),
                                6 to ScanningLayer(4))),
                got)
    }

    @Test
    fun testStepShouldIncreaseLayerOfPacket() {
        val scanner = PacketScanner.getScannerForInputFile("sample_input.txt")
        scanner.step()
        assertEquals("Packet should now be in layer 0", 0, scanner.currentLayerOfPacket)
    }

    @Test
    fun testStepShouldMoveScanners() {
        val scanner = PacketScanner.getScannerForInputFile("sample_input.txt")
        scanner.step()
        assertEquals("should increase pos by 1 if direction = forward",
                ScanningLayer(range = 3, scannerDirection = 1, scannerPosition = 1),
                scanner.layers[0])
        scanner.step()
        assertEquals("should still increase pos by 1 if direction = forward",
                ScanningLayer(range = 3, scannerDirection = 1, scannerPosition = 2),
                scanner.layers[0])
        scanner.step()
        assertEquals("should change direction and decrease pos by 1 if at max",
                ScanningLayer(range = 3, scannerDirection = -1, scannerPosition = 1),
                scanner.layers[0])
        scanner.step()
        assertEquals("should still decrease pos by 1",
                ScanningLayer(range = 3, scannerDirection = -1, scannerPosition = 0),
                scanner.layers[0])
        scanner.step()
        assertEquals("should change direction and increase pos by 1 if at 0",
                ScanningLayer(range = 3, scannerDirection = 1, scannerPosition = 1),
                scanner.layers[0])
    }

    @Test
    fun testStepShouldUpdateListOfViolations() {
        val scanner = PacketScanner.getScannerForInputFile("sample_input.txt")
        scanner.step()
        assertEquals("first violation on step 1, since scanner on layer 0 is at pos 0",
                hashMapOf(0 to 3), scanner.violations)
        scanner.step()
        assertEquals("no violation on step 2, since scanner on layer 1 is at pos 1",
                hashMapOf(0 to 3), scanner.violations)
        scanner.step()
        assertEquals("no violation on step 3, since there is no scanner on layer 2",
                hashMapOf(0 to 3), scanner.violations)
        scanner.step()
        assertEquals("no violation on step 3, since there is no scanner on layer 3",
                hashMapOf(0 to 3), scanner.violations)
        scanner.step()
        assertEquals("no violation on step 4, since scanner on layer 4 is elsewhere",
                hashMapOf(0 to 3), scanner.violations)
        scanner.step()
        assertEquals("no violation on step 3, since there is no scanner on layer 5",
                hashMapOf(0 to 3), scanner.violations)
        scanner.step()
        assertEquals("second violation on step 6, since scanner on layer 6 is at pos 0",
                hashMapOf(0 to 3, 6 to 4), scanner.violations)
    }

    @Test
    fun testSolveShouldReturnCorrectSolutionForExample() {
        val scanner = PacketScanner.getScannerForInputFile("sample_input.txt")
        assertEquals("severity for sample input: 0*3 + 6*4 = 24", 24, scanner.solve())
    }

    @Test
    fun testSolveShouldSolvePartI() {
        val scanner = PacketScanner.getScannerForInputFile("input.txt")
        assertEquals("severity for puzzle input", 1876, scanner.solve())
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