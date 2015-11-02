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
}
