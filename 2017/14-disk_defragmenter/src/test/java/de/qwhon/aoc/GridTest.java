package de.qwhon.aoc;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

/**
 * Created by frank on 04.02.18.
 */
public class GridTest {

    // A grid consisting of two disconnected cells
    @Test
    public void componentCountShouldReturn2For4x4GridWithTwoDisconnectedCells() {
        List<String> contents = Arrays.asList("1", "0", "0", "1");
        Grid grid = new Grid(contents);
        assertEquals(2, grid.getConnectedComponentCount());
    }


    // A U-shaped grid that is connected in the last row
    @Test
    public void componentCountShouldReturn1For4x4GridThatIsUShaped() {
        List<String> contents = Arrays.asList("9", "9", "9", "F");
        Grid grid = new Grid(contents);
        assertEquals(1, grid.getConnectedComponentCount());
    }
}
