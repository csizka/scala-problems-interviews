package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object Parentheses extends App{

  def hasValidPatentheses(str: String): Boolean = {
    @tailrec
    def validPhHelper(string: String, acc: (Int, Int)): Boolean = {
      val (opening, closing) = acc
      if (closing > opening) false
      else if (!string.isEmpty && string.head == '(') validPhHelper(string.tail, (opening + 1, closing))
      else if (!string.isEmpty && string.head == ')') validPhHelper(string.tail, (opening, closing + 1))
      else if (!string.isEmpty) validPhHelper(string.tail, acc)
      else opening == closing
    }
    validPhHelper(str, (0,0))
  }
  val goodTst1 = "()"
  val goodTst2 = "()()()"
  val goodTst3 = "((())())"
  val goodTst4 = "(e(dd  (dsfa)d )d(df)f)"
  val nGoodTst1 = ")("
  val nGoodTst2 = "()dsjd("
  val nGoodTst3 = "(j sa ()nbs nb))"
  val nGoodTst4 = "(sajh sakc"
  val nGoodTst5 = "()shacc ((jsaj)ksa Cks"
  assert(hasValidPatentheses(goodTst1))
  assert(hasValidPatentheses(goodTst2))
  assert(hasValidPatentheses(goodTst3))
  assert(hasValidPatentheses(goodTst4))
  assert(!hasValidPatentheses(nGoodTst1))
  assert(!hasValidPatentheses(nGoodTst2))
  assert(!hasValidPatentheses(nGoodTst3))
  assert(!hasValidPatentheses(nGoodTst4))
  assert(!hasValidPatentheses(nGoodTst5))

}

