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
  "Exercise 3.3 List.setHead" - {
    "should add item to front of list" in {
      List.setHead(1, List(2,3,4,5)) should be(List(1,2,3,4,5))
    }
    "should return list with item if list is Nil to start" in {
      List.setHead(1, Nil) should be(List(1))
    }
  }
  "Exercise 3.4 List.drop" - {
    "should drop the first n elements from the list" in {
      List.drop(Nil, 1) should be(Nil)
      List.drop(List("foo","bar","baz","buzz","xyzzy"), 2) should be(List("baz", "buzz", "xyzzy"))
    }
  }
  "Exercise 3.5 List.dropWhile" - {
    "drops items that match the predicate from the left side of the list" in {
      List.dropWhile(List(1,2,3,4,5), (n: Int) => (n < 4)) should be(List(4,5))
    }
    "predicate matching stops after the first false occurs" in {
      List.dropWhile(List(1,2,3,4,1,5), (n: Int) => (n < 4)) should be(List(4,1,5))
    }
  }
  "Exercise 3.6 List.init" - {
    "retuns a list of all but the last element of the given list" in {
      List.init(List(1,2,3,4)) should be (List(1,2,3))
    }
  }

  "3.8 replace constructors with replacements for the replacements of the constructors" in {
    val theList = List(1,2,3,4,5)
    List.foldRight(theList, Nil: List[Int])((x,y) => Cons(x,y)) should be(theList)
  }
}
