package com.actualadam.fpis.ch3

import org.scalatest._

class Ch3_Notes extends FreeSpec with Matchers {
  "Pattern Matching on Lists" - {
    "variable matching pattern using _" in {
      val x = List(1,2,3) match { case _ => 42 }
      x should be (42)
    }
    "Capturing a subexpression of the target" - {
      "capture the head" in {
        val x = List(1,2,3) match { case Cons(h,_) => h }
        x should be (1)
      }
      "capture the tail" in {
        val x = List(1,2,3) match { case Cons(_,t) => t }
        x should be (List(2,3))
      }
    }
    "Match error occurs when no cases match" in {
      intercept[MatchError] {
        val x = List(1,2,3) match { case Nil => 42 }
      }
    }
  }
}
