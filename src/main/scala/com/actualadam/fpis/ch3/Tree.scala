package com.actualadam.fpis.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /*
  * Returns the number of nodes in the tree
  */
  def size(t: Tree): Int = {
    def go(t: Tree, acc: Int): Int = match t {
      case Leaf(_) => 1
      case Branch(l,r) => go(l, acc + 1)  
    }

    }
  }

}
