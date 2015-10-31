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

  "3.8 passing Nil and Cons (the constructors of List) to a foldRight is the identity of a List" in {
    val theList = List(1,2,3,4,5)
    List.identity(theList) should be (theList)
  }

  "3.9 - Compute the length of a list using foldRight" in {
    List.length(List(1,2,3,4,5)) should be (5)
  }

  "3.10, 3.11 - FoldLeft" in {
    List.sumFoldl(List(1,2,3,4,5)) should be (15)
    List.productFoldl(List(1,2,3,4,5)) should be (120)
  }

  "3.12 - reverse using fold" in {
    List.reverse(List(1,2,3,4,5)) should be (List(5,4,3,2,1))
  }

  "3.13 is hard"

  "3.14 - append using foldr" in {
    val list1 = List(1,2,3)
    val list2 = List(2,3,4,5)
    List.appendFoldr(list1, list2) should be (List(1,2,3,2,3,4,5))
  }

  "3.15 - concatenate turns a list of lists into a flat list" in {
    List.concat(List(List(1,1),List(2,2),List(3,3))) should be (List(1,1,2,2,3,3))
  }

  "3.16 - function that transforms a list of integers by adding 1 to each element" in {
    List.mapAddOne(List(1,2,3)) should be (List(2,3,4))
  }

  "3.17 - function that transforms a list of doubles to a list of strings" in {
    List.mapDoubleToString(List(1.1,1.2,1.3)) should be (List("1.1","1.2","1.3"))
  }

  "3.18 - map" in {
    List.map(List(1,2,3))((x) => x + 1) should be (List(2,3,4))
    List.map(List(1.1,1.2,1.3))((d) => d.toString) should be (List("1.1","1.2","1.3"))
  }

  "3.19 - filter can remove all odd numbers from a list of integers" in {
    List.filter(List(2,3,5,6,9,12,21,56))((x) => x % 2 == 0) should be (List(2,6,12,56))
  }

  "3.20 - flatMap maps each element to 0 or more elements" in {
    List.flatMap(List(1,2,3))((i) => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "3.21 - filter with flatMap" in {
    List.filterWithFlatMap(List(2,3,5,6,9,12,21,56))((x) => x % 2 == 0) should be (List(2,6,12,56))
  }

  "3.22 - zip with addition" in {
    List.zipWithAddition(List(1,2,3), List(4,5,6)) should be (List(5,7,9))
  }

  "3.23 - zipWith ..." in {
    List.zipWith(List(1,2,3), List(4,5,6))((a,b) => a + b) should be (List(5,7,9))
  }

  "3.24 is hard"

  "3.25 Tree.size counts all the nodes in a given tree" - {
    val testTree: Tree[Int] = Branch(Branch(Leaf(1),Leaf(1)),Branch(Branch(Leaf(1),Leaf(1)),Leaf(1)))
    Tree.size(testTree) should be (9)
    Tree.size2(testTree) should be (9)
  }

  "3.26 Tree.max returns the greatest value in a Tree[Int]" - {
    "root node is a branch" in {
      val testTree: Tree[Int] = Branch(Branch(Leaf(1),Leaf(4)),Branch(Branch(Leaf(8),Leaf(2)),Leaf(3)))
      Tree.maximum(testTree) should be (8)
    }
    "root node is a leaf" in {
      Tree.maximum(Leaf(42)) should be (42)
    }
  }

  "3.27 Tree.depth returns the maximum path length from the root node of a Tree to any leaf" - {
    "root node is a leaf" in {
      Tree.depth(Leaf(42)) should be (0)
    }
    "root node is a branch" in {
      Tree.depth(Branch(Leaf(42), Leaf(24))) should be (1)
      Tree.depth(Branch(Branch(Branch(Branch(Branch(Leaf(1),Leaf(1)), Leaf(1)), Leaf(1)), Leaf(1)), Leaf(1))) should be (5)
      Tree.depth(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))) should be (3)
    }
  }

  "3.28 Tree.map creates a new tree by applying a function to each element" in {
    Tree.map(Branch(Branch(Leaf(1), Leaf(4)), Leaf(2)))((x) => x + 1) should be (Branch(Branch(Leaf(2), Leaf(5)), Leaf(3)))
  }




}
