package com.rockthejvm.solutions.nums

import scala.annotation.tailrec

object reverseInteger extends App {

  def reverseInt(num: Int): Int = {
    @tailrec
    def intReverser(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else {
        val digits = n % 10
        val tentativeRes = acc * 10 + digits
        if (tentativeRes >= 0) intReverser(n / 10, tentativeRes)
        else 0
      }
    }
    if (num == Int.MinValue) 0
    else if (num > 0) intReverser(num, 0)
    else -intReverser(-num,0)
  }
  println(reverseInt(54388680))
  println(reverseInt(-1234567))
  println(reverseInt(Int.MinValue))
  println(Int.MinValue)
  println(reverseInt(0))
  println(reverseInt(-045))
  println(reverseInt(Int.MaxValue))
  println(Int.MaxValue)

}
