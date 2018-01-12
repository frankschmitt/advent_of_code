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

data class ScannerRecord(val depth: Int, val range: Int)

data class PacketScanner(var currentLayerOfPacket: Int, val layers: Map<Int, ScanningLayer>,
                         var violations: HashMap<Int,Int> = hashMapOf()) {

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

        /** compute the minimum delay necessary to pass through the firewall without getting caught.
         *    we return the first number that fulfills:
         *       for every scanner N: scanner_N is not at position 0 at timestamp (depth_N + delay)
         *       since (depth_N + delay) is the timestamp at which our packet reaches level N, this solves our puzzle
         */
        fun minimumDelayForInputFile(fileName: String): Int {
            val input = parseInputFile(fileName)
            val scanners = input.map { ScannerRecord(depth = it.key, range = it.value) }
            return generateSequence(seed = 0) { i -> i + 1 }
                    .find { i -> scanners.none { scanner -> (i + scanner.depth) % ((scanner.range - 1) * 2) == 0 } }!!
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
        updateViolations()
        moveScanners()
    }

    /** Solve the puzzle, and return the severity
     *    The severity is the sum of the violation weights; the weight of each violation is its depth multiplied by its range
     *
     */
    fun solve(): Int {
        val maxLayer = layers.keys.max() ?: -1
        while (currentLayerOfPacket <= maxLayer) {
            step()
        }
        return violations.toList().fold(initial = 0) { accu, (depth,range) -> accu + depth * range }
    }

    private fun updateViolations() {
        if (layers.contains(currentLayerOfPacket) && (layers.getValue(currentLayerOfPacket).scannerPosition == 0)) {
            violations[currentLayerOfPacket] = layers.getValue(currentLayerOfPacket).range
        }
    }

    private fun movePacket() {
        currentLayerOfPacket += 1
    }
}


fun main(args: Array<String>) {
    val scanner = PacketScanner.getScannerForInputFile("input.txt")
    println("part I: ${scanner.solve()}")
}

