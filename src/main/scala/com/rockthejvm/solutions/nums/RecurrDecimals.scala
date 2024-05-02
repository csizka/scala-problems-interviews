package com.rockthejvm.solutions.nums

import scala.annotation.tailrec

object RecurrDecimals extends App {
  def fractionRecurDecimals(numerator: Long, denominator: Long): String = {
    val quotient0 = numerator / denominator
    val remainder0 = numerator % denominator
    @tailrec
    def recurIndex(curRem: Long, remList: List[Long], curIx: Int): Option[Int] = {
      if (remList.isEmpty) None
      else if (curRem == remList.head) Some(curIx)
      else recurIndex(curRem, remList.tail, curIx + 1)
    }
    @tailrec
    def f2d(num: Long, denom: Long, quot: List[Long], rem: List[Long]): String = {
      val quotient = num * 10 / denom
      val remaining = num * 10 % denom
      val recurIxOption = recurIndex(remaining, rem, 0)

      if (remaining == 0) (quot :+ quotient).mkString("")
      else if (recurIxOption.isEmpty) f2d(remaining, denom, quot :+ quotient, rem :+ remaining)
      else {
        val (beforRecur, recur) = (quot :+ quotient).splitAt(recurIxOption.get)
        s"${beforRecur.mkString("")}(${recur.mkString("")})"
      }
    }
    if (remainder0 == 0) quotient0.toString
    else if (numerator / denominator.toDouble < 0) s"-$quotient0.${f2d(Math.abs(remainder0), Math.abs(denominator), List.empty, List.empty)}"
    else s"$quotient0.${f2d(remainder0, denominator, List.empty, List(remainder0))}"
  }
  println(fractionRecurDecimals(-1,-333))
  println(1/333.0)
}
