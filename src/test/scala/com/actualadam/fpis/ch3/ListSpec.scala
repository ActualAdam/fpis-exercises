package com.actualadam.fpis.ch3

import org.scalatest._
import com.actualadam.fpis.ch3.List._

class ListSpec extends FreeSpec with Matchers {
  "Using List I can" - {
    "Cons a list of Ints" in {
      val ints: List[Int] = Cons(1, Cons(2, Cons(4, Nil)))
    }
    "Instantiate as Nil no matter what type the List is" in {
      val nilDouble: List[Double] = Nil
      val nilInt: List[Int] = Nil
      nilDouble should equal(nilInt)
    }
  }

  "The fill function" - {
    "should return Nil when n is 0" in {
      fill(0, "e") should be(Nil)
    }
    "should return a list of 5 es when n is 5 and a is e" - {
      fill(5, "e") should be(Cons("e", Cons("e", Cons("e", Cons("e", Cons("e", Nil))))))
    }
  }

}
