package com.actualadam.fpis.ch2

import annotation.tailrec

object Listing22 {
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }
}
