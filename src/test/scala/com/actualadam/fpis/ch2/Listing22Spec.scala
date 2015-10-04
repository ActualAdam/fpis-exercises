package com.actualadam.fpis.ch2

import org.scalatest._

class Listing22Spec extends FreeSpec with Matchers {
  "factorial" - {
    "performs the factorial operation" in {
      Listing22 factorial 4 should be (24)
    }
  }
}
