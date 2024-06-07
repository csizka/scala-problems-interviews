package com.rockthejvm.solutions.nums

import scala.annotation.tailrec

object RecurrDecimals extends App {
  def fractionToRecurringDecimals(numerator: Long, denominator: Long): String = {
    val quotient0 = numerator / denominator
    val remainder0 = numerator % denominator
    @tailrec
    def recurIndex(curRem: Long, prevRems: List[Long], curIx: Int): Option[Int] = {
      if (prevRems.isEmpty) None
      else if (curRem == prevRems.head) Some(curIx)
      else recurIndex(curRem, prevRems.tail, curIx + 1)
    }
    @tailrec
    def f2d(num: Long, denom: Long, previousQuots: List[Long], prevRemains: List[Long]): String = {
      val quotient = num * 10 / denom
      val remaining = num * 10 % denom
      val recurIxOption = recurIndex(remaining, prevRemains, 0)

      if (remaining == 0) (previousQuots :+ quotient).mkString("")
      else if (recurIxOption.isEmpty) f2d(remaining, denom, previousQuots :+ quotient, prevRemains :+ remaining)
      else {
        val (beforeRecur, recur) = (previousQuots :+ quotient).splitAt(recurIxOption.get)
        s"${beforeRecur.mkString("")}(${recur.mkString("")})"
      }
    }
    if (remainder0 == 0) quotient0.toString
    else if (numerator / denominator.toDouble < 0) s"-$quotient0.${f2d(Math.abs(remainder0), Math.abs(denominator), List.empty, List.empty)}"
    else s"$quotient0.${f2d(remainder0, denominator, List.empty, List(remainder0))}"
  }
  println(fractionToRecurringDecimals(-1,-333))
  println(1/333.0)
  println(fractionToRecurringDecimals(1,6))
  println(1/6.0)
  println(fractionToRecurringDecimals(1, 1023))
  println(1 / 1023.0)
  println(fractionToRecurringDecimals(1, 13))
  println(1 / 13.0)
}
