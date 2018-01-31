package de.qwhon.aoc;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
//import knot_hash.core;

public class DiskDefragmenter {

    public static void main(String[] args) {

        // NB: our namespace knot-hash contains a hyphen.  This is translated to
        // an underscore in the file path.  It can be tricky to remember when to use a
        // hyphen and when to use an underscore, so maybe a better practice is to
        // avoid hypens in namespaces
        IFn require = Clojure.var("clojure.core","require");
        require.invoke(Clojure.read("knot_hash.core"));

        IFn computeHash = Clojure.var("knot-hash.core", "compute-hash-part-II");
        computeHash.invoke("xyz");
        // TODO: how can we get the result? We get back a knot_hash.core.KnotHashState
        //       but Java doesn't know this type
        //knot_hash.core.KnotHashState state = computeHash.invoke("xyz");
        //clojure.lang.IFn plus = Clojure.var("clojure.core", "+");
        //plus.invoke(1, 2);
        System.out.println("It works!");
    }
  }

