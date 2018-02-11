package de.qwhon.aoc;

import java.util.List;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import org.junit.Ignore;
import static org.junit.Assert.assertNotNull;

public class DiskDefragmenterTest {

    @Test
    public void countBitsInHexStringShouldReturn9ForSampleInput() {
        // 10100000110000100000000101110000
        assertEquals(9, Grid.countBitsInHexString("a0c2017"));
    }
   @Test
    public void gridForSampleInputShouldBuildTheGridCorrectly() {
       DiskDefragmenter defragmenter = new DiskDefragmenter();
       String input = "flqrgnkx";
       Grid grid = defragmenter.gridFor(input);
       List<String> contents = grid.getContents();
       assertEquals(128, contents.size());
       // check first + last hash code
       String expected0 = defragmenter.computeHash(input + "-0");
       assertEquals(expected0, contents.get(0));
       String expected127 = defragmenter.computeHash(input + "-127");
       assertEquals(expected127, contents.get(127));
       // check grid count
       assertEquals(8108, grid.getOccupiedCellCount());
       // check component count
       int got = grid.getConnectedComponentCount();
       grid.writeToFile(("sample-grid.txt"));
       assertEquals(1242, got);
   }
}
