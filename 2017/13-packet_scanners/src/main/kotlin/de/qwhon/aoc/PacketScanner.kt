package de.qwhon.aoc

import java.io.File

/** A simple helper class for keeping track of scanners.
 *
 */
data class ScannerRecord(val depth: Int, val range: Int)


/** parse the input file, and return a List of scanner records.
 *
 */
fun parseInputFile(fileName: String): List<ScannerRecord> {
    val file = File(fileName)
    val tmp = HashMap<Int, Int>()
    file.forEachLine {
        val contents = it.split(": ")
        tmp[contents[0].toInt()] = contents[1].toInt()
    }
    return tmp.map { ScannerRecord(depth = it.key, range = it.value) }
}

/** compute the minimum delay necessary to pass through the firewall without getting caught.
 *    we return the first number that fulfills:
 *       for every scanner N: scanner_N is not at position 0 at timestamp (depth_N + delay)
 *       since (depth_N + delay) is the timestamp at which our packet reaches level N, this solves our puzzle
 */
fun minimumDelayForInputFile(fileName: String): Int {
    val input = parseInputFile(fileName)
    return generateSequence(seed = 0) { i -> i + 1 }
            .find { i -> input.none { scanner -> (i + scanner.depth) % ((scanner.range - 1) * 2) == 0 } }!!
}

/** compute the severity of the violations for passing through the firewall without initial delay.
 *       we compute the severity as:
 *         for every scanner N: violation occurs iff scanner_N is at position 0 at timestamp (depth_N + delay)
 *         since (depth_N + delay) is the timestamp at which our packet reaches level N, this returns all scanners
 *         that catch our packet. Computing the severity is then a simple sum of depth*range, which can be computed by a fold.
 */
fun severityOfViolationsForInputFile(fileName: String): Int {
    val input = parseInputFile(fileName)
    return input.filter { scanner -> scanner.depth % ((scanner.range - 1) * 2) == 0 }
            .fold(initial = 0) { accu, scanner -> accu + scanner.depth * scanner.range }
}


