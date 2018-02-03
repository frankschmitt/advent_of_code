package de.qwhon.aoc;

import java.util.List;
import java.util.ArrayList;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
//import clojure.lang.AFn;
//import knot_hash.core;
//import knot_hash.core.KnotHashState;

public class DiskDefragmenter {

    IFn solve2;
    IFn computeHash;

    public DiskDefragmenter() {
        // NB: our namespace knot-hash contains a hyphen.  This is translated to
        // an underscore in the file path.  It can be tricky to remember when to use a
        // hyphen and when to use an underscore, so maybe a better practice is to
        // avoid hypens in namespaces
        IFn require = Clojure.var("clojure.core","require");
        require.invoke(Clojure.read("knot_hash.core"));

        computeHash = Clojure.var("knot-hash.core", "compute-hash-part-II");
        solve2 = Clojure.var("knot-hash.core", "solve2");
    }

    public static void main(String[] args) {
        DiskDefragmenter defragmenter = new DiskDefragmenter();
        defragmenter.computeHash.invoke("xyz");
        // TODO: how can we get the result? We get back a knot_hash.core.KnotHashState
        //       but Java doesn't know this type
        //knot_hash.core.KnotHashState state = (knot_hash.core.KnotHashState) computeHash.invoke("xyz");

        String s = (String) defragmenter.solve2.invoke("197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63");
        //clojure.lang.IFn plus = Clojure.var("clojure.core", "+");
        //plus.invoke(1, 2);
        System.out.println("It works!" + s);
    }

    public static List<String> gridFor(String input) {
        return new ArrayList<String>();
    }
  }

