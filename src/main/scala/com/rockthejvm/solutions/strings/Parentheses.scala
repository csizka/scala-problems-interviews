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

  def genAllValidParenths(n: Int): Set[String] = {
    @tailrec
    def genHelper(num: Int, acc: Set[String]): Set[String] = {
      if (num == n) acc
      else {
        val curStrings = for {
          elem <- acc
          index <- 0 to elem.length
        } yield {
          val (before,after) = elem.splitAt(index)
          before + "()" + after
        }
        genHelper(num + 1, curStrings)
      }
    }
    if (n <=0) Set{"Pls add a valid num."}
    genHelper(1, Set("()"))
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

  assert(genAllValidParenths(1) == Set("()"))
  assert(genAllValidParenths(2) == Set("(())", "()()"))
  assert(genAllValidParenths(3) == Set("((()))", "(()())", "()(())", "()()()", "(())()"))
  assert(genAllValidParenths(4) == Set("(((())))", "()((()))", "(()(()))", "((()()))", "((())())", "((()))()", "()(()())", "(()()())", "(()())()", "()()()()", "()(())()", "(())()()", "()()(())", "(())(())"))


}

