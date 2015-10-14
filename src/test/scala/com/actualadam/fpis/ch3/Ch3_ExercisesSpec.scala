package com.actualadam.fpis.ch3

import org.scalatest._
import com.actualadam.fpis.ch3.List._

class Ch3_Exercises extends FreeSpec with Matchers {
  "Exercise 3.1" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x should be (3)
  }
  "Exercise 3.2 List.tail" - {
      "should return Nil for Nil" in {
        List.tail(Nil) should be(Nil)
      }
      "should return Nil for 1 item list" in {
        List.tail(List(1)) should be(Nil)
      }
      "should return all items except the first item" in {
        List.tail(List(1,2,3,4,5)) should be(List(2,3,4,5))
      }
    }
  }
  "Exercise 3.3 List.setHead" - {
      "should add item to front of list" in {
        List.setHead(1, List(2,3,4,5)) should be(List(1,2,3,4,5))
      }
      "should return list with item if list is Nil to start" in {
        List.setHead(1, Nil) should be(List(1))
      }
    }
  }
  // "Exercise 3.4 List.drop" - {
  //   "should drop the first n elements from the list" in {
  //     List.drop(1, Nil)
  //   }
  //
  // }
}
