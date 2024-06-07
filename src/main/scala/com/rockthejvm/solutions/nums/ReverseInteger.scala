package com.rockthejvm.solutions.nums

import scala.annotation.tailrec

object ReverseInteger extends App {

  // return a number with the digits reversed
  // if the result overflows Int(in either direction), return 0
  def reverseInt(num: Int): Int = {
    @tailrec
    def reverseIntHelper(curNum: Int, acc: Int): Int = {
      if (curNum == 0) acc
      else {
        val digits = curNum % 10
        val tentativeRes = acc * 10 + digits
        if (tentativeRes >= 0) reverseIntHelper(curNum / 10, tentativeRes)
        else 0
      }
    }
    if (num == Int.MinValue) 0
    else if (num > 0) reverseIntHelper(num, 0)
    else -reverseIntHelper(-num,0)
  }
  assert(reverseInt(54388680) == 8688345)
  assert(reverseInt(-1234567) == -7654321)
  assert(reverseInt(-045) == -54)
  assert(reverseInt(0) == 0)
  assert(reverseInt(Int.MinValue) == 0)
  assert(reverseInt(Int.MaxValue) == 0)

}
