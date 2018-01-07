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
    }

    @Test
    fun testStepShouldSetNumberOfViolations() {
        val scanner = PacketScanner.getScannerForInputFile("sample_input.txt")
        scanner.step()
        assertEquals("first violation on step 1, since scanner on layer 0 is at pos 0", 1, scanner.numberOfViolations)
        scanner.step()
        assertEquals("no violation on step 2, since scanner on layer 1 is at pos 1", 1, scanner.numberOfViolations)
        scanner.step()
        assertEquals("no violation on step 3, since there is no scanner on layer 2", 1, scanner.numberOfViolations)
    }


}