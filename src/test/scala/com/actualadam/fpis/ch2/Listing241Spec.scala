package com.actualadam.fpis.ch2

import org.scalatest._

class Listing241Spec extends FreeSpec with Matchers {
  "factorial" - {
    "performs the factorial operation" in {
      Listing241 factorial 4 should be (24)
    }
  }
}
