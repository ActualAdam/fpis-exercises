package com.actualadam.fpis.ch3

import annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def fill[A](n: Int, a: A): List[A] = {
    def loop(n: Int, a: A, as: List[A]): List[A] = {
      if (n <= 0) as
      else loop(n - 1, a, Cons(a,as))
    }
    loop(n, a, Nil)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => as
    case Cons(x,xs) => xs
  }

  def setHead[A](a: A, as: List[A]): List[A] = {
    Cons(a,as)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}