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
         *   We simply initialize PacketScanners with currentLayerOfPacket = -1, -2, ..., run them and check for violations
         */
        fun minimumDelayForInputFile(fileName: String): Int {
            val input = parseInputFile(fileName)
         /*   val layers = input.mapValues { ScanningLayer(range = it.value) }
            return (0..1000000).find {
                val scanner = PacketScanner(currentLayerOfPacket = -1 - it, layers = layers)
                scanner.solve()
                println("testing delay $it")
                scanner.violations.isEmpty()
            }!! - 1
         */
            //val sequences: List<Sequence<Int>> = input.map { it -> generateScannerSequence(range = it.value, depth = it.key)}
            //return generateSequence(seed = 0) { i -> i + 1}
                    //.minus(sequences.first()).first()!!
            //        .find { i -> sequences.none { seq -> seq.takeWhile{ j -> j <= i}.contains(i)}}!!
            val scanners = input.map { ScannerRecord(depth = it.key, range = it.value)}
            return generateSequence(seed = 0) { i -> i + 1}
                   .find { i -> scanners.none { scanner -> (i + scanner.depth) % ((scanner.range-1)*2) == 0}}!!
        }

        /** generate the sequence for the given scanner range and depth
         *   Generates the timestamps at which the scanner is at pos 0
         *   Example: for a scanner of depth 0 and range 3,
         *     its positions are:   0-1-2-1-0-1-2-1-0-...
         *     and the timestamps:  0-1-2-3-4-5-6-7-8-...
         *     and the sequence is: 0       4       8
         *   If depth is non-zero, we simply subtract it from the generated sequence (with shift):
         *     depth = 0, range = 3:   0-1-2-1-0-1-2-1-0
         *     depth = 1, range = 4: 0-1-2-3-4-3-2-1-0-1
         *   Every column is then a possible path through the firewall. To find the path that allows us to pass
         *   without getting caught, we only have to find the leftmost column where no position is 0.
         *   If transformed to the sequence space, it is equivalent to finding the first number n that is contained
         *   in none of the shifted sequences.
         */
        fun generateScannerSequence(range: Int, depth: Int = 0): Sequence<Int> =
            generateSequence(seed = -depth) { it -> it + (range-1)*2 }

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

