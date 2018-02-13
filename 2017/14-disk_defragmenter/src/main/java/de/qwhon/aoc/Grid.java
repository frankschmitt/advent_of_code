package de.qwhon.aoc;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.jgrapht.*;
import org.jgrapht.graph.*;
import org.jgrapht.alg.ConnectivityInspector;

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
        // count bits in hex strings
        return contents.stream().map(s -> Grid.countBitsInHexString(s)).reduce(0, (accu, val) -> accu + val);
    }

    /**
     * Print a grid row to a string.
     * Cells are separated by a dot '.'
     * @param row Grid row
     * @param width Width for each cell, e.g. width = 4 -> print(1) = "0001"
     * @return String representation of the grid row
     */
    private String printRow(int[] row, int width) {
        //return Arrays.stream(row).reduce("",
        //                                 (accu, val) -> accu + ((Integer)val).toString(),
        //                                 (accu1, accu2) -> accu1 + accu2);
        // this is stupid. Why is the Java8 stream API so broken, i.e. does not allow reduce: Container<T> -> U ?
        String s = "";
        String format = "%0" + ((Integer) width).toString() + "d.";
        for (int i : row) {
            s += String.format(format, i);
        }
        return s;
    }

    /**
     * Print a complete grid to a string.
     * Rows are separated by newlines.
     * @param width Width for each cell, @see printRow()
     * @return String representation of the gird
     */
    public String printGrid(int width) {
        return Arrays.stream(this.matrix)
                .map(row -> printRow(row, width))
                .reduce("", (accu, val) -> accu + val + "\n");
    }

    /**
     * Write the String representation of the grid to the given text file.
     * @param fileName Name of output file
     * @param width Width of each cell, @see printRow()
     */
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
     * @param s input string in Hex (uppercase, lowercase and mixedCase are all ok)
     * @return binary representation of the string
     */
    public static int[] hexToBinary(String s) {
        // this is stupid - using Java8 streams would have been much nicer, but I gave up on it
        //   since it has too many restrictions, e.g. no reduce :: Stream<T> -> U
        //   (i.e. you cannot reduce a Stream to a different type without also specifying a (useless) combiner function)
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
                    // stupidly return nonsense, since apparently Java is too brain-dead to recognize an exception thrown inside
                    // map() correctly
                    res.append("XXXX");
                    break;
            }
        }
        // 48: ASCII code of '0', i.e. map '0' -> 0, '1' -> 1
        return res.toString().chars().map(c -> c - 48).toArray();
    }

    /**
     * Convert our grid contents from a list of hex strings to a 2D-Array of ints
     */
    private void buildGridAsIntMatrix() {
        this.matrix = contents.stream()
                // convert each hex string to a binary string
                .map(s -> hexToBinary(s)
                )
                .toArray(int[][]::new);
    }

    /**
     * Get a unique label for the given node
     * @param row row of the node (in the grid)
     * @param col column of the node (in the grid)
     * @return unique label
     */
    private int nodeLabel(int row, int col) {
        // HACK we rely on the fact that our grid is never larger than 999x999 cells
        return row*1000 + col;
    }

    /**
     * Create an undirected graph representing our grid.
     * For each occupied cell, we add a node, and for each connection between occupied neighbour cells,
     * we add an edge.
     * @return the result graph
     */
    private Graph<Integer, DefaultEdge> createGraph()
    {
        Graph<Integer, DefaultEdge> g = new SimpleGraph<>(DefaultEdge.class);

        for (int i = 0; i < this.matrix.length; ++i) {
            int[] row = this.matrix[i];
            for (int j = 0; j < row.length; ++j) {
                // Ignore 0 cells
                if (row[j] != 0) {
                    // add node
                    g.addVertex(nodeLabel(i, j));
                    // not topmost row? check top neighbour + add edge if it's present
                    if ((i > 0) && (matrix[i-1][j] != 0)) {
                        g.addEdge(nodeLabel(i,j), nodeLabel(i-1,j));
                    }
                    // not leftmost column? check left neighbour + add edge if it's present
                    if ((j > 0) && (matrix[i][j-1] != 0)) {
                        g.addEdge(nodeLabel(i,j), nodeLabel(i, j-1));
                    }
                }
            }
        }

        return g;
    }

    /**
     * Get the number of connected components
     *
     * @return the number of connected components
     */
    public int getConnectedComponentCount() {
        buildGridAsIntMatrix();
        Graph<Integer, DefaultEdge> g = createGraph();
        ConnectivityInspector<Integer, DefaultEdge> inspector = new ConnectivityInspector<>(g);
        return inspector.connectedSets().size();
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
