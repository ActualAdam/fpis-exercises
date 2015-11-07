package com.actualadam.fpis.ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
