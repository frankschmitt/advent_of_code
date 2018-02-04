package de.qwhon.aoc;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.stream.*;
import java.util.stream.Collectors;

/**
 * Created by frank on 03.02.18.
 */
public class Grid {

    List<String> contents;

    public Grid(List<String> contents) {
        this.contents = contents;
    }

    /**
     * Get the grid contents as a list of hexadecimal strings.
     * Each string represents one row in the grid.
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
        return contents.stream().map(s -> Grid.countBitsInHexString(s)).reduce(0, (accu, val) -> accu + val);
    }

    private String printGrid(int[][] grid) {
        return Arrays.stream(grid).map(row -> Arrays.toString(row)).reduce("", (accu, val) -> accu + "\n" + val);
    }

    /**
     * Get the number of connected components
     * @return the number of connected components
     */
    public int getConnectedComponentCount() {
        // convert our grid contents from a list of hex strings to a 2D-Array of ints
        //List<String> contentsInBinary = contents.stream().map(s -> new BigInteger(s, 16).toString(2)).collect(Collectors.toList());
        Stream<IntStream> tmp = contents.stream().map(s -> new BigInteger(s, 16).toString(2).chars());
        Stream<int[]> tmp2 = tmp.map(s -> s.toArray());
        int[][] tmp3 = tmp2.toArray(int[][]::new);
        System.out.println("tmp3: " + printGrid(tmp3));
        //Stream<IntStream > tmp2 = tmp.map(s -> s.chars());
        //Integer[][] tmp3 = (Integer[][]) tmp2.map(a -> a.toArray()).toArray(Integer[][]::new);
        //String[] array = Stream.of( ... ).toArray( String[]::new );

        //char[][] contentsInBinary = contents
        //        .stream().map(s -> new BigInteger(s, 16).toString(2))
        //        .map(s -> s.toCharArray())
        //        .toArray();
        return 1;
    }

    /**
     * Count the number of 1 bits in a hex string
     * @param input Number in hexadecimal notation
     * @return Number of 1 bits in the input
     */
    public static int countBitsInHexString(String input) {
        return new BigInteger(input, 16).bitCount();
    }
}
