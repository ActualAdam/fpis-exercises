package com.actualadam.fpis.ch2

import annotation.tailrec

object Exercise21 {
  /*
  * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). The first two Fibonacci numbers
  * are 0 and 1. The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your
  * definition should use a local tail-recursive function.
  */
  def fib(n: Int): Int = {
    if (n < 1) return -1 // Is validation a side effect?
    @tailrec
    def go(n: Int, prev: Int, acc: Int): Int = {
      println(s"n: $n, prev: $prev, acc: $acc") // side effect I don't know how to deal with: debug I/O
      if (n <= 0) acc
      else go(n - 1, acc, acc + prev)
    }
    go(n, -1, 1)
  }
}
