package com.actualadam.fpis.ch4

import org.scalatest._

class Ch4 extends FreeSpec with Matchers {
  "Example of mean, a function that returns the average value of all the values in a Seq, using Option" - {
    def mean(ds: Seq[Double]): Option[Double] =
      if (ds.isEmpty) None
      else Some(ds.sum / ds.length)

    "non-empty ds should return a mean" in {
      mean(Seq(1.0,3.0,5.0)) should be (Some(3.0))
    }
    "empty ds should return None" in {
      mean(Seq()) should be (None)
    }
  }

  "Exercise 4.1: Implement these functions on Option." - {
    "map: map still maps 1:1. It just doesn't apply the function in tha case of None." - {
      def plusOne(x: Int) = x + 1 // a test function that map will optionally be applied.
      "function is applied when the option type is Some" in {
        Some(4).map(plusOne) should be (Some(5))
      }
      "function is not applied when the option type is None" in {
        None.map(plusOne) should be (None)
      }
    }
    "flatMap acepts a function that emits an option and flattens" - {
      def plusOneSometimes(x: Int): Option[Int] = if (x == 4) Some(x + 1) else None
      "maps a function return of Some to a flat Some instead of a Some(Some(x))" in {
        Some(4).flatMap(plusOneSometimes) should be (Some(5))
      }
      "maps a function return of None to a flat None" in {
        Some(5).flatMap(plusOneSometimes) should be (None)
      }
    }
    "getOrElse" - {}
    "orElse" - {}
    "filter" - {}
  }
}
