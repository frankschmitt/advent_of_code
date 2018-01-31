package de.qwhon.aoc;

import clojure.java.api.Clojure;

public class DiskDefragmenter {

    public static void main(String[] args) {
        // TODO: this doesn't work yet!
      clojure.lang.IFn computeHash = Clojure.var("knot-hash.core", "compute-hash-part-II");
      computeHash.invoke("xyz");
      System.out.println("It works!");
    }
  }

