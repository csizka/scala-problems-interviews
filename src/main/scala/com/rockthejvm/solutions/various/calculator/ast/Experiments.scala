package com.rockthejvm.solutions.various.calculator.ast

object Experiments {

  def comp[A, B, C](f: A => B, g: B => C): A => C = { (a: A) => g(f(a)) }

  def comp2[A, B, C](f: A => B, g: B => C, a: A): C = {
    g(f(a))
  }


}
