package de.qwhon.aoc;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.Ignore;
import static org.junit.Assert.assertEquals;

/**
 * Created by frank on 04.02.18.
 */
public class GridTest {

    @Test
    public void hexToBinaryShouldPadStringCorrectly() {
        assertEquals("[0, 0, 0, 0]", Arrays.toString(Grid.hexToBinary("0")));
        assertEquals("[0, 0, 0, 1]", Arrays.toString(Grid.hexToBinary("1")));
        assertEquals("[0, 0, 0, 0, 0, 0, 0, 0]", Arrays.toString(Grid.hexToBinary("00")));
    }

    // A grid consisting of two disconnected cells
    @Test 
    public void componentCountShouldReturn2For4x4GridWithTwoDisconnectedCells() {
        List<String> contents = Arrays.asList("8", "0", "0", "8");
        Grid grid = new Grid(contents);
        assertEquals(2, grid.getConnectedComponentCount());
    }

    // A U-shaped grid that is connected in the last row
    @Test
    public void componentCountShouldReturn1For4x4GridThatIsUShaped() {
        List<String> contents = Arrays.asList("9", "9", "9", "F");
        Grid grid = new Grid(contents);
        int got = grid.getConnectedComponentCount();
        grid.writeToFile("U-shaped-grid.txt");
        assertEquals(1, got);
    }
}
