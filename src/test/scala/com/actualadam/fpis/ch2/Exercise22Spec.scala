package com.actualadam.fpis.ch2

import org.scalatest._
import Exercise22._
import scala.math.Ordered

class Exercise22Spec extends FreeSpec with Matchers {
  "isSorted" - {
    "handles simple cases" - {
      "given a array if Ints" - {
        def compareIntAscending(l: Int, r: Int): Boolean = l <= r
        "in ascending order and an ascending comparator" in {
          isSorted(Array(1, 2, 3, 4), compareIntAscending) should be (true)
        }
        "in descending order and an ascending comparator" in {
          isSorted(Array(4, 3, 2, 1), compareIntAscending) should be (false)
        }
      }
    }

    "handles different comparators" - {
      "these streets" - {
        val streets = Array("Flanders", "Lovejoy", "Quimby")
        "should be sorted alphabetically" in {
          isSorted(streets, (l: String, r: String) => l.charAt(0) <= r.charAt(0)) should be (true)
        }
        "should not be sorted reverse-alphabetically" in {
          isSorted(streets, (l: String, r: String) => l.charAt(0) >= r.charAt(0)) should be (false)
        }
        "should not be sorted by ascending length" in {
          isSorted(streets, (l: String, r: String) => l.length <= r.length) should be (false)
        }
        "should be sorted by descending length" in {
          isSorted(streets, (l: String, r: String) => l.length >= r.length) should be (true)
        }
      }
    }
  }
}
