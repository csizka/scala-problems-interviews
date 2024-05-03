package com.rockthejvm.solutions.nums

import scala.annotation.tailrec

object ParseInteger extends App {

  def parseInt(str: String): Int = {
    val set = (0 to 9).toSet
    @tailrec
    def recurParse(string: String, acc: Int, sign: Int): Int = {
      lazy val char = string.charAt(0) - 48
      lazy val partialRes = acc * 10 + char
      if (string == "") sign * acc
      else if (set.contains(char) && partialRes >= 0) recurParse(string.drop(1), partialRes, sign)
      else if (set.contains(char) && partialRes < 0 && sign > 0) Int.MaxValue
      else if (set.contains(char) && partialRes < 0 && sign < 0) Int.MinValue
      else if (string.charAt(0) == '-') recurParse(string.drop(1), acc, -1)
      else recurParse(string.drop(1), acc, sign)
    }
    recurParse(str, 0, 1)
  }

assert(parseInt("  -  2471299678973") == Int.MinValue)
assert(parseInt("  -  0") == 0)
assert(parseInt(" +  973 912 ") == 973912)
assert(parseInt("  247129 9789999 3") == Int.MaxValue)
assert(parseInt("  -  56kdgi23932") == -5623932)
}
