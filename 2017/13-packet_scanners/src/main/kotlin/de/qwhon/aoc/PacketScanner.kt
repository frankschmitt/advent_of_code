package de.qwhon.aoc

import java.io.File

data class ScanningLayer(
        val range: Int,
        var scannerDirection: Int = 1, // 1: upward, -1: downward
        var scannerPosition: Int = 0   // starting position
) {
    companion object {
        val UPWARD = 1
        val DOWNWARD = -1;
    }

    fun move() {
        if (scannerDirection == UPWARD && scannerPosition == (range - 1)) {
            scannerDirection = DOWNWARD
        }
        if (scannerDirection == DOWNWARD && scannerPosition == 0) {
            scannerDirection = UPWARD
        }
        scannerPosition += scannerDirection
    }
}


//data class Layer(val range: Int, var direction: Int, var scannerPosition: Int)

data class PacketScanner(var currentLayerOfPacket: Int, var numberOfViolations: Int = 0, val layers: Map<Int, ScanningLayer>) {

    companion object {

        /** parse the input file, and return a HashMap containing the layers and their depth.
         *
         */
        fun parseInputFile(fileName: String): HashMap<Int, Int> {
            val file = File(fileName)
            val result = HashMap<Int, Int>()
            file.forEachLine {
                val contents = it.split(": ")
                result[contents[0].toInt()] = contents[1].toInt()
            }
            return result
        }

        /** create a scanner for the given input file.
         *
         * @param fileName Name of the input file containing the layer specs (format: <layer>: <depth>, e.g. "0: 4")
         * @return packet scanner with initial state
         */
        fun getScannerForInputFile(fileName: String): PacketScanner {
            val input = parseInputFile(fileName)
            val layers = input.mapValues { ScanningLayer(range = it.value) }
            return PacketScanner(currentLayerOfPacket = -1, layers = layers)
        }
    }

    /** Move the scanners.
     *    Each scanner moves one step forward; if it is at the minimum / maximum position, it changes its direction
     */
    fun moveScanners() {
        for (scanner in layers.values) {
            scanner.move()
        }
    }

    /** Perform the next step
     *
     */
    fun step(): Unit {
        movePacket()
        updateNumberOfViolations()
        moveScanners()
    }

    private fun updateNumberOfViolations() {
        if (layers.contains(currentLayerOfPacket) && (layers.getValue(currentLayerOfPacket).scannerPosition == 0)) {
            numberOfViolations += 1
        }
    }

    private fun movePacket() {
        currentLayerOfPacket += 1
    }
}


fun main(args: Array<String>) {
    println("do it")
}
