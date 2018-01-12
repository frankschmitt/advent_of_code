package de.qwhon.aoc

import java.io.File

data class ScannerRecord(val depth: Int, val range: Int)

class PacketScanner {

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

        fun severityOfViolationsForInputFile(fileName: String): Int {
            val input = parseInputFile(fileName)
            val scanners = input.map { ScannerRecord(depth = it.key, range = it.value) }
            return scanners.filter { scanner -> scanner.depth % ((scanner.range-1)*2) == 0}
                    .fold(initial = 0) {accu, scanner -> accu + scanner.depth * scanner.range}
        }
    }

}


fun main(args: Array<String>) {
}

