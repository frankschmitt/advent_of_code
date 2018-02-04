package de.qwhon.aoc;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
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

    private String leftPadWithZeros(String s) {
        String result;
        if (s.length() % 8 != 0) {
            result = "00000000".substring(s.length() % 8) + s;
        } else {
            result = s;
        }
        return result;
    }

    private int[] hexToBinary(String s, int length) {
        String binary = new BigInteger(s, 16)
                // convert to binary representation
                .toString(2);
        String padded = leftPadWithZeros(binary);
        return  padded.chars()
                // 48 = ASCII code of zero, i.e. we map '0' -> 0, '1' -> 1
                .map(i-> i - 48)
                .toArray();
    }

    /** Convert our grid contents from a list of hex strings to a 2D-Array of ints
     *
     * @return A NxN 2D array containing 1s and 0s containing the binary representation of our grid
     */
    private int[][] getGridAsIntMatrix() {
        int cnt = contents.size();
        //java.text.DecimalFormat decimalFormat = new java.text.DecimalFormat();
        //decimalFormat.setMinimumIntegerDigits(cnt);
        return contents.stream()
                // convert each hex string to a binary string
                .map(s -> hexToBinary(s, cnt)
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
            if (matrix[row][col-1] != 0) {
                result.add(matrix[row][col-1]);
            }
        }
        // no need to check right / bottom neighbour - these weren't visited yet
        return result.toArray(new Integer[0]);
    }

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
     * @return the number of connected components
     */
    public int getConnectedComponentCount() {
        //List<String> contentsInBinary = contents.stream().map(s -> new BigInteger(s, 16).toString(2)).collect(Collectors.toList());
        this.matrix = getGridAsIntMatrix();
        //System.out.println("matrix before: " + printGrid(matrix));

        //Set<Integer> components = new Set<Integer();
        int currIndex = 1;
        int cnt = 0;
        for(int i = 0; i < matrix.length; ++i) {
            int[] row = matrix[i];
            for (int j = 0; j < row.length; ++j) {
                // ignore 0 cells
                if (row[j] != 0) {
                    Integer[] neighbourComponents = getNeighbourComponents(matrix, i, j);
                    // no neighbour set? use current index + increase it
                    if (neighbourComponents.length == 0) {
                        matrix[i][j] = currIndex++;
                        cnt++;
                    }
                    // one neighbour component: use its component
                    else if (neighbourComponents.length == 1) {
                        matrix[i][j] = neighbourComponents[0];
                    }
                    // two neighbour components: use the first component, and re-number all neighbours to use this component
                    else {
                        matrix[i][j] = neighbourComponents[0];
                        renumberGridCells(neighbourComponents, i, j);
                        cnt--;
                    }
                }
            }
        }
        //System.out.println("matrix after: " + printGrid(matrix));

        return cnt;
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
