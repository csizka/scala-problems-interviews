package com.rockthejvm.solutions.strings

import scala.annotation.tailrec
import scala.collection.immutable.WrappedString

object Parentheses extends App{

  def hasValidParentheses(str: String): Boolean = {
    val endIx = str.length - 1
    val parenths = str.toSeq
    @tailrec
    def validPhHelper(string: WrappedString, opening: Int, closing: Int, curIx: Int): Boolean = {
      if (closing > opening) false
      else {
        val (counted, toCount) = string.splitAt(curIx)
        toCount match {
          case '(' +: restChars => validPhHelper(string, opening + 1, closing, curIx + 1)
          case ')' +: restChars => validPhHelper(string, opening, closing + 1, curIx + 1)
          case WrappedString.empty => opening == closing
          case _ => validPhHelper(string, opening, closing, curIx + 1)
        }
      }
    }
    validPhHelper(parenths, 0, 0, 0)
  }
  def hasValidParenthesesV2(str: String): Boolean = {
    val (canBeValid, openingPhCount, closingPhCount) = str.foldLeft((true, 0, 0)) { case ((canBValid, curOpening, curClosing), curPharentheses) =>
      val nextOpeningCount = if (curPharentheses == '(') curOpening + 1 else curOpening
      val nextClosingCount = if (curPharentheses == ')') curClosing + 1 else curClosing
      val nextCanBValid = canBValid && nextClosingCount <= nextOpeningCount
    if (!nextCanBValid) (false, 0, 0)
    else (true, nextOpeningCount, nextClosingCount)
    }
    canBeValid && openingPhCount == closingPhCount
  }

  def genAllValidParenths(n: Int): Set[String] = {
    @tailrec
    def genHelper(parenthsToAdd: Int, curNumOfParenthsOptions: Set[String]): Set[String] = {
      if (parenthsToAdd == n) curNumOfParenthsOptions
      else {
        val curStrings = for {
          elem <- curNumOfParenthsOptions
          index <- 0 to elem.length
        } yield {
          val (before,after) = elem.splitAt(index)
          before + "()" + after
        }
        genHelper(parenthsToAdd + 1, curStrings)
      }
    }
    if (n <= 0) throw new Exception(s" please add a number, that is begger than zero. You wrote: '$n'")
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

  assert(hasValidParenthesesV2(goodTst1))
  assert(hasValidParenthesesV2(goodTst2))
  assert(hasValidParenthesesV2(goodTst3))
  assert(hasValidParenthesesV2(goodTst4))
  assert(!hasValidParenthesesV2(nGoodTst1))
  assert(!hasValidParenthesesV2(nGoodTst2))
  assert(!hasValidParenthesesV2(nGoodTst3))
  assert(!hasValidParenthesesV2(nGoodTst4))
  assert(!hasValidParenthesesV2(nGoodTst5))
  assert(hasValidParenthesesV2(goodTst1))
  assert(hasValidParentheses(goodTst2))
  assert(hasValidParentheses(goodTst3))
  assert(hasValidParentheses(goodTst4))
  assert(!hasValidParentheses(nGoodTst1))
  assert(!hasValidParentheses(nGoodTst2))
  assert(!hasValidParentheses(nGoodTst3))
  assert(!hasValidParentheses(nGoodTst4))
  assert(!hasValidParentheses(nGoodTst5))


  assert(genAllValidParenths(1) == Set("()"))
  assert(genAllValidParenths(2) == Set("(())", "()()"))
  assert(genAllValidParenths(3) == Set("((()))", "(()())", "()(())", "()()()", "(())()"))
  assert(genAllValidParenths(4) == Set("(((())))", "()((()))", "(()(()))", "((()()))", "((())())", "((()))()", "()(()())", "(()()())", "(()())()", "()()()()", "()(())()", "(())()()", "()()(())", "(())(())"))


}

