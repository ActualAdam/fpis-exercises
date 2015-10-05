package com.actualadam.fpis.ch2

import annotation.tailrec

object Listing {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def monomorphicFindFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](as: Array[A], key: A): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (as(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }
}
