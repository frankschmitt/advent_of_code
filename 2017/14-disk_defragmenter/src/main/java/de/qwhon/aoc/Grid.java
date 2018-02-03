package de.qwhon.aoc;

import java.math.BigInteger;
import java.util.List;
import java.util.stream.*;

/**
 * Created by frank on 03.02.18.
 */
public class Grid {

    List<String> contents;

    public Grid(List<String> contents) {
        this.contents = contents;
    }

    public List<String> getContents() {
        return contents;
    }

    public int getCount() {
        return contents.stream().map(s -> Grid.countBitsInHexString(s)).reduce(0, (accu, val) -> accu + val);
    }

    public static int countBitsInHexString(String input) {
        return new BigInteger(input, 16).bitCount();
    }
}
