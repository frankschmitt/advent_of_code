package de.qwhon.aoc;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.stream.*;
import java.util.stream.Collectors;

/**
 * Created by frank on 03.02.18.
 */
public class Grid {

    List<String> contents;
    int[][] matrix;

    public Grid(List<String> contents) {
        this.contents = contents;
    }

    /**
     * Get the grid contents as a list of hexadecimal strings.
     * Each string represents one row in the grid.
     *
     * @return List of grid rows in hexadecimal notation
     */
    public List<String> getContents() {
        return contents;
    }

    /**
     * Get the count of 1 bits in the grid contents (= number of occupied grid cells)
     *
     * @return Number of 1 bits in the binary notation of the grid contents
     */
    public int getOccupiedCellCount() {
        // quick variant: count bits in hex strings
        //return contents.stream().map(s -> Grid.countBitsInHexString(s)).reduce(0, (accu, val) -> accu + val);
        // slow variant: build int matrix, count non-0 cells
        buildGridAsIntMatrix();
        int cnt = 0;
        for (int[] row: matrix) {
            for (int val: row) {
                if (val != 0) {
                    ++cnt;
                }
            }
        }
        return cnt;
    }

    private String printRow(int[] row, int width) {
        //return Arrays.stream(row).reduce("",
        //                                 (accu, val) -> accu + ((Integer)val).toString(),
        //                                 (accu1, accu2) -> accu1 + accu2);
        // this is stupid. Why is the Java8 stream API so broken?
        String s = "";
        String format = "%0" + ((Integer)width).toString() + "d.";
        for (int i : row) {
            //s += ((Integer) i).toString() + ".";
            s += String.format(format, i);
        }
        return s;
    }

    public String printGrid(int width) {
        //return Arrays.stream(this.matrix).map(row -> Arrays.toString(row)).reduce("", (accu, val) -> accu + "\n" + val);
        return Arrays.stream(this.matrix)
                //.map(row -> Arrays.stream(row).reduce("", (accu, val) -> accu + ((Integer)val).toString()))
                .map(row -> printRow(row, width))
                .reduce("", (accu, val) -> accu + val + "\n");
    }

    public void writeToFile(String fileName, int width) {
        try (PrintWriter out = new PrintWriter(fileName)) {
            out.println(printGrid(width));
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.toString());
        }
    }

    /**
     * Convert a hex string to its binary representation (as int array)
     *
     * @param s
     * @return
     */
    public static int[] hexToBinary(String s) {
        // this is stupid - Streams would have been much nicer, but I gave up on it
        //   since it has too many restrictions, e.g. no reduce :: Stream<T> -> U -> fn: U, T -> U -> U, i.e. you
        //   cannot reduce a Stream to a different type, and many more
        StringBuilder res = new StringBuilder();
        for (char ch : s.toUpperCase().toCharArray()) {
            switch (ch) {
                case '0':
                    res.append("0000");
                    break;
                case '1':
                    res.append("0001");
                    break;
                case '2':
                    res.append("0010");
                    break;
                case '3':
                    res.append("0011");
                    break;
                case '4':
                    res.append("0100");
                    break;
                case '5':
                    res.append("0101");
                    break;
                case '6':
                    res.append("0110");
                    break;
                case '7':
                    res.append("0111");
                    break;
                case '8':
                    res.append("1000");
                    break;
                case '9':
                    res.append("1001");
                    break;
                case 'A':
                    res.append("1010");
                    break;
                case 'B':
                    res.append("1011");
                    break;
                case 'C':
                    res.append("1100");
                    break;
                case 'D':
                    res.append("1101");
                    break;
                case 'E':
                    res.append("1110");
                    break;
                case 'F':
                    res.append("1111");
                    break;
                default:
                    // stupidly return nonsense, since Java is too brain-dead to recognize its exception handlers
                    res.append("XXXX");
                    break;
            }
        }
        //return res.toString().chars()
        //        .map(c -> if (c == '0') { 0 } else { 1 })
        //           .toArray(int[][]::new);
        // 48: ASCII code of '0'
        return res.toString().chars().map(c -> c - 48).toArray();
    }

    /**
     * Convert our grid contents from a list of hex strings to a 2D-Array of ints
     *
     */
    private void buildGridAsIntMatrix() {
        // this is the most stupid language restriction ever - I declare that the function may throw an Exception,
        //   but the Java compiler still complains? WTF?
        this.matrix = contents.stream()
                // convert each hex string to a binary string
                .map(s -> hexToBinary(s)
                )
                //.map(s -> s.toArray())
                .toArray(int[][]::new);
    }

    private Integer[] getNeighbourComponents(int[][] matrix, int row, int col) {
        Set<Integer> result = new HashSet<Integer>();
        if (row > 0) {
            // not topmost row? add neighbour at top if it isn't zero
            if (matrix[row - 1][col] != 0) {
                result.add(matrix[row - 1][col]);
            }
        }
        // not leftmost col? add neighbour at left if it isn't zero
        if (col > 0) {
            if (matrix[row][col - 1] != 0) {
                result.add(matrix[row][col - 1]);
            }
        }
        // no need to check right / bottom neighbour - these weren't visited yet
        // sort the components to ensure components with lower indices come first
        Integer[] tmp = result.toArray(new Integer[0]);
        Arrays.sort(tmp);
        return tmp;
    }

    /** Renumber the grid cells (merge two distinct areas)
     *
     * @param neighbourComponents Grid areas we want to merge (#1 gets renumbered to #0)
     * @param uptoRow we renumber all grid rows from 0 .. uptoRow
     * @param uptoCol we renumber all grid cols from 0 .. uptoCol
     */
    private void renumberGridCells(Integer[] neighbourComponents, int uptoRow, int uptoCol) {
        for (int i = 0; i <= uptoRow; ++i) {
            for (int j = 0; j <= uptoCol; ++j) {
                if (this.matrix[i][j] == neighbourComponents[1]) {
                    this.matrix[i][j] = neighbourComponents[0];
                }
            }
        }
    }

    /**
     * Get the number of connected components
     *
     * @return the number of connected components
     */
    public int getConnectedComponentCount() {
        //List<String> contentsInBinary = contents.stream().map(s -> new BigInteger(s, 16).toString(2)).collect(Collectors.toList());
        buildGridAsIntMatrix();
        //System.out.println("matrix before: " + printGrid(matrix));

        //Set<Integer> components = new Set<Integer();
        int currIndex = 1;
        int cnt = 0;
        for (int i = 0; i < matrix.length; ++i) {
            int[] row = matrix[i];
            for (int j = 0; j < row.length; ++j) {
                // ignore 0 cells
                if (row[j] != 0) {
                    Integer[] neighbourComponents = getNeighbourComponents(matrix, i, j);
                    // no neighbour set? use current index + increase it
                    if (neighbourComponents.length == 0) {
                        matrix[i][j] = currIndex++;
                        cnt++;
                        if (i == 127) writeToFile(String.format("debug/sample-%03d-%03d_standalone.txt", i, j), 4);
                    }
                    // one neighbour component: use its component
                    else if (neighbourComponents.length == 1) {
                        matrix[i][j] = neighbourComponents[0];
                        if (i == 127) writeToFile(String.format("debug/sample-%03d-%03d_neighbour-%03d.txt", i, j,
                                neighbourComponents[0]), 4);
                    }
                    // two neighbour components: use the first component, and re-number all neighbours to use this component
                    else {
                        matrix[i][j] = neighbourComponents[0];
                        renumberGridCells(neighbourComponents, i, j);
                        cnt--;
                        if (i == 127) writeToFile(String.format("debug/sample-%03d-%03d_merged-%03d-%03d.txt", i, j,
                                neighbourComponents[0], neighbourComponents[1]), 4);
                    }
                }
            }
        }
        //System.out.println("matrix after: " + printGrid(matrix));
        // count the disjunct areas
        Set<Integer> result = new  HashSet<Integer>();
        for (int[] row: matrix) {
            for (int val: row) {
                if (val != 0) {
                    result.add(val);
                }
            }
        }
        return result.size();
        //return cnt;
    }

    /**
     * Count the number of 1 bits in a hex string
     *
     * @param input Number in hexadecimal notation
     * @return Number of 1 bits in the input
     */
    public static int countBitsInHexString(String input) {
        return new BigInteger(input, 16).bitCount();
    }
}
