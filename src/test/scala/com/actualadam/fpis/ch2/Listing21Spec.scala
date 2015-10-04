package com.actualadam.fpis.ch2

import org.scalatest._

class Listing21Spec extends FreeSpec with Matchers {
  "FormatAbs" - {
    "displays absolute values in English" in {
      Listing21.formatAbs(-4) should be(
        "The absolute value of -4 is 4")
    }
  }

  "Abs" - {
    "performs the absolute value operation" in {
      Listing21.abs(-42) should be(42)
      Listing21.abs(42) should be(42)
      Listing21.abs(0) should be(0)
    }
  }

  "To access an object member, I can" - {
    "use dot notation" in {
      Listing21.abs(-42) should be(42)
    }
    "import an object member and use it without specifying the full path" in {
      import Listing21.abs
      abs(-42) should be(42)
    }
    "have scoped imports. That's interesting!" in {
      if ("just creating a block of scope".length > 0) {
        import Listing21.abs // import is scoped to this block
        abs(-42) should be(42)
      }
      Listing21.abs(-42) should be(42) // I must use the dot notation outside the block
    }
    "use infix notation when I feel like it" in {
      Listing21 abs -42 should be(42)
    }
    "import all of an object's members using _  as a wildcard" in {
      import Listing21._
      abs(-42) should be(42)
    }
  }

  "Why can't I..." - {
    "use infix notation and imported members together?" ignore {
      import Listing21._
      // abs - 42 should be(42)
    }
  }
}
