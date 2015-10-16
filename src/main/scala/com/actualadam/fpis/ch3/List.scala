package com.actualadam.fpis.ch3

import annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  /*
  * Returns the sum of each Int in the given list.
  */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  /*
  * Returns the product of each double in the given list.
  */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  /*
  * Creates a list with n elements of a.
  */
  def fill[A](n: Int, a: A): List[A] = {
    def loop(n: Int, a: A, as: List[A]): List[A] = {
      if (n <= 0) as
      else loop(n - 1, a, Cons(a,as))
    }
    loop(n, a, Nil)
  }

  /**
  * Returns the tail of the given list. A new List with the first element of the original list removed.
  */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => as
    case Cons(x,xs) => xs
  }

  /*
  * Removes the first n elements of the given list.
  */
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  /*
  * Removes elements from the begining of the list if they match the given predicate. Once the first element that
  * doesn't match is encountered, no more elements are removed.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /*
  * Appends the elements of the a2 to a1.
  */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  /*
  * Add an element to the front of the given list.
  */
  def setHead[A](a: A, as: List[A]): List[A] = {
    Cons(a,as)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
