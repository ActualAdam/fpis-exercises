package com.actualadam.fpis.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /*
  * Returns the number of nodes in the tree.
  * My first attempt, which appears to work.
  */
  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(l,r) => {
        go(l, go(r, acc + 1))
      }
    }
    go(t, 0)
  }

  /*
  * The answer key's more straightforward solution.
  */
  def size2[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  /*
  * Returns the maximum element in a Tree[Int]
  */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  /*
  * Returns the maximum path length from the root of a Tree to any leaf.
  */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  /*
  * Creates a new tree by applying a function to each element of the given Tree.
  */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }
}
