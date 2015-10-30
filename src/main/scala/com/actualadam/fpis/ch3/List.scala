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

  def dropWhileCurried[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhileCurried(t)(f)
    case _ => as
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

  /*
  * Removes an element from the right side of the list.
  */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumFoldr(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def productFoldr(ns: List[Int]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_,acc) => acc + 1)
  }

  def identity[A](as: List[A]): List[A] =
    foldRight(as, Nil:List[A])(Cons(_,_))

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def sumFoldl(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productFoldl(ns: List[Int]) =
    foldLeft(ns, 1.0)((x,y) => x * y)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((x,y) => Cons(y,x))

  def appendFoldr[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons(_,_))

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])(append)

  def mapAddOne(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((h,t) => Cons(h + 1,t))

  def mapDoubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h,t) => Cons(h.toString, t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a) => if (f(a)) Cons(a,Nil) else Nil)

  def zipWithAddition[Int](ns1: )


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
