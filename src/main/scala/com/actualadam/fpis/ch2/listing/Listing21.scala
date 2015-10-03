package com.actualadam.fpis.ch2.listing

object Listing21 {
    def abs(n: Int): Int =
        if (n < 0) -n
        else n

    def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }
}
