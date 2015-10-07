package com.actualadam.fpis.ch2

import annotation.tailrec

object Exercise22 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      val orderedThese = ordered(as(n), as(n + 1))
      if (n == as.length - 2) orderedThese
      else if (orderedThese) go(n + 1)
      else false
    }
    go(0)
  }
}
