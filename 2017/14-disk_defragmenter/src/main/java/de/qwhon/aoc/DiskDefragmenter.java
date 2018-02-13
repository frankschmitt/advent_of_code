package de.qwhon.aoc;

import java.util.List;
import java.util.ArrayList;
import java.util.stream.*;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class DiskDefragmenter {

    IFn solve2_;

    public DiskDefragmenter() {
        // NB: our namespace knot-hash contains a hyphen.  This is translated to
        // an underscore in the file path.  It can be tricky to remember when to use a
        // hyphen and when to use an underscore, so maybe a better practice is to
        // avoid hypens in namespaces
        IFn require = Clojure.var("clojure.core","require");
        require.invoke(Clojure.read("knot_hash.core"));

        solve2_ = Clojure.var("knot-hash.core", "solve2");
    }

    public static void main(String[] args) throws Exception {
        DiskDefragmenter defragmenter = new DiskDefragmenter();
        Grid grid = defragmenter.gridFor("jzgqcdpd");
        System.out.println("part I: " + grid.getOccupiedCellCount()); // 8074
        System.out.println("part II: " + grid.getConnectedComponentCount()); // 1212
    }

    public String computeHash(String input) {
        return (String) this.solve2_.invoke(input);
    }

    public Grid gridFor(String input) {
        List<String> contents = IntStream.iterate(0, i -> i + 1)
                .limit(128)
                .mapToObj(i -> computeHash(input + "-" + (Integer)i).toString())
                .collect(Collectors.toList());
        return new Grid(contents);
    }
  }

