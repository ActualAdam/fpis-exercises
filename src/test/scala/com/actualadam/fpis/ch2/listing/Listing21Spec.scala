package com.actualadam.fpis.ch2.listing

import org.scalatest._

class Listing21Spec extends FlatSpec with Matchers {
    "Listing21" should "display absolute values in English" in {
        Listing21.formatAbs(-4) should be (
          "The absolute value of -4 is 4"
        )
    }

    it should "perform the simple calculation" in {
      Listing21.abs(-4) should be (
        4
      )
      Listing21.abs(4) should be (
        4
      )
    }
}
