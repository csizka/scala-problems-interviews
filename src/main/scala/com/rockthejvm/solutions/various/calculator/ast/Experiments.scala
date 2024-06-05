package com.rockthejvm.solutions.various.calculator.ast

object Experiments extends App {

  def comp[A, B, C](f: A => B, g: B => C): A => C = { (a: A) => g(f(a)) }

  def comp2[A, B, C](f: A => B, g: B => C, a: A): C = {
    g(f(a))
  }

  // we need the +s for the type params so that:
  // if A1 <: A2, and B1 <: B2, then Either[A1, B1] <; Either[A2, B2]
  // this is also whz we need the +s in Left and Right
  sealed trait Either[+L, +R]{
    def map[R2](f: R => R2): Either[L, R2]

    def flatMap[L1 >: L, R2](f: R => Either[L1, R2]): Either[L1, R2]
  }

  case class Left[+L, +R](value: L) extends Either[L, R] {
    override def map[R2](f: R => R2): Either[L, R2] = Left(value)
    override def flatMap[L1 >: L, R2](f: R => Either[L1, R2]): Either[L1, R2] = Left(value)
  }
  case class Right[+L, +R](value: R) extends Either[L, R] {
    override def map[R2](f: R => R2): Either[L, R2] = Right(f(value))
    override def flatMap[L1 >: L, R2](f: R => Either[L1, R2]): Either[L1, R2] = f(value)
  }

  object Either {
    def left[L, R](l: L): Either[L, R] = Left(l)
  }

  {
    def div2(n: Int): Either[String, Int] = if (n % 2 == 0) Right(n / 2) else Left("input is odd")

    assert(Left("some error").flatMap(div2) == Left("some error"))
    assert(Right(6).flatMap(div2) == Right(3))
    assert(Right(3).flatMap(div2) == Left("input is odd"))
    assert(Right(6).flatMap(div2).flatMap(div2) == Left("input is odd"))
  }
}
