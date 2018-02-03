package de.qwhon.aoc;

import java.util.List;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class DiskDefragmenterTest {

   @Test
    public void testGridForSampleInput() {
       List<String> grid = DiskDefragmenter.gridFor("flqrgnkx");
       assertEquals(128, grid.size());
   }
}
