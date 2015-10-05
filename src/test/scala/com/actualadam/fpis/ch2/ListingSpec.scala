package com.actualadam.fpis.ch2

import org.scalatest._

class ListingSpec extends FreeSpec with Matchers {
  "FormatAbs" - {
    "displays absolute values in English" in {
      Listing.formatAbs(-4) should be(
        "The absolute value of -4 is 4")
    }
  }

  "Abs" - {
    "performs the absolute value operation" in {
      Listing.abs(-42) should be(42)
      Listing.abs(42) should be(42)
      Listing.abs(0) should be(0)
    }
  }

  "factorial" - {
    "performs the factorial operation" in {
      Listing factorial 4 should be (24)
    }
  }

  "To access an object member, I can" - {
    "use dot notation" in {
      Listing.abs(-42) should be(42)
    }
    "import an object member and use it without specifying the full path" in {
      import Listing.abs
      abs(-42) should be(42)
    }
    "have scoped imports. That's interesting!" in {
      if ("just creating a block of scope".length > 0) {
        import Listing.abs // import is scoped to this block
        abs(-42) should be(42)
      }
      Listing.abs(-42) should be(42) // I must use the dot notation outside the block
    }
    "use infix notation when I feel like it" in {
      Listing abs -42 should be(42)
    }
    "import all of an object's members using _  as a wildcard" in {
      import Listing._
      abs(-42) should be(42)
    }
  }

  "formatFactorial" - {
    "prints the factorial of x in English" in {
      Listing.formatFactorial(4) should be (
        "The factorial of 4 is 24"
      )
    }
  }

  "formatResult" - {
    import Listing._
    "should format an abs call" in {
      formatResult("absolute value", -42, abs) should be (
        "The absolute value of -42 is 42"
      )
    }
    "should format a factorial call" in {
      formatResult("factorial", 4, factorial) should be (
        "The factorial of 4 is 24"
      )
    }
  }

val homeKeys = Array("a", "s", "d", "f", "j", "k", "l", ";")

  "monomorphicFindFirst" - {
    import Listing._
    "finds the index of the first occurance of a string item in a given array of strings" in {
      monomorphicFindFirst(homeKeys, "f") should be (3)
    }
    "returns -1 if no strings in the array match the given key" in {
      monomorphicFindFirst(homeKeys, "z") should be (-1)
    }
  }

 "polymorphicFindFirst" - {
   import Listing._
   "works on Strings" in {
     findFirst(homeKeys, "j") should be (4)
   }
   "works on Ints" in {
     findFirst(Array(1, 2, 3, 4, 5), 3) should be (2)
   }
 }

  "Why can't I..." - {
    "use infix notation and imported members together?" ignore {
      import Listing._
      // abs -42 should be(42)   // interestingly my editor's scala formatter put a space between - and 42.
                                 // what does it think I'm trying to do?

      // abs 42 should be(42)    // this doesn't work either and

      // My new guess is that infix notation is intended (solely) to appear to be an operation on 2 operands even though
      // it's really calling a function of the first operand using the second operand as an argument.  Since there's
      // no appearance of a first operand here, it's not actually infix notation as such.
    }
  }
}
