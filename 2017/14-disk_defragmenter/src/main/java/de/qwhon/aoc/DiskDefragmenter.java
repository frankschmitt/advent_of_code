package de.qwhon.aoc;

import java.util.List;
import java.util.ArrayList;
import java.util.stream.*;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
//import clojure.lang.AFn;
//import knot_hash.core;
//import knot_hash.core.KnotHashState;

public class DiskDefragmenter {

    IFn solve2_;
    IFn computeHash_;

    public DiskDefragmenter() {
        // NB: our namespace knot-hash contains a hyphen.  This is translated to
        // an underscore in the file path.  It can be tricky to remember when to use a
        // hyphen and when to use an underscore, so maybe a better practice is to
        // avoid hypens in namespaces
        IFn require = Clojure.var("clojure.core","require");
        require.invoke(Clojure.read("knot_hash.core"));

        computeHash_ = Clojure.var("knot-hash.core", "compute-hash-part-II");
        solve2_ = Clojure.var("knot-hash.core", "solve2");
    }

    public static void main(String[] args) throws Exception {
        DiskDefragmenter defragmenter = new DiskDefragmenter();
        //defragmenter.computeHash_.invoke("xyz");
        // TODO: how can we get the result? We get back a knot_hash.core.KnotHashState
        //       but Java doesn't know this type
        //knot_hash.core.KnotHashState state = (knot_hash.core.KnotHashState) computeHash.invoke("xyz");

        //String s = (String) defragmenter.solve2_.invoke("197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63");
        //System.out.println("It works!" + s);
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

