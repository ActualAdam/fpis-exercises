package com.actualadam.fpis.ch2

import org.scalatest._
import Exercise21._

/*
* Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). The first two Fibonacci numbers
* are 0 and 1. The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your
* definition should use a local tail-recursive function.
*/
class Excercise21Spec extends FlatSpec with Matchers {
  "The fib function" should "be able to calculate the 1st Fibonacci number" in {
    fib(1) should be (0)
  }
  it should "be able to calculate the 2nd Fibonacci number" in {
    fib(2) should be (1)
  }
  it should "be able to calculate the nth Fibonacci number" in {
    fib(3) should be (1)
    fib(4) should be (2)
    fib(5) should be (3)
    fib(6) should be (5)
  }
  it should "return -1 if n is invalid" in {
    fib(-1) should be (-1)
    fib(0) should be (-1)
  }
}
