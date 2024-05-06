package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object CountChars extends App {

  def countChars(str:String): Map[Char, Int] = {
    @tailrec
    def charChecker(string: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (string.isEmpty) acc
      else {
        val curChar = string.charAt(0)
        val charCount = acc.getOrElse(curChar, 0)
        val newElement = (curChar, charCount + 1)
        charChecker(string.drop(1), acc + newElement)
      }
    }
    charChecker(str, Map())
  }

  def countCharsV2(str: String): Map[Char, Int] = {
    str.foldLeft(Map.empty: Map[Char, Int])((map: Map[Char, Int], char: Char) => map + (char -> (map.getOrElse(char, 0) + 1)))
  }
  val testStr = "random checker 1 aaa a 2 1"
  println(countChars(testStr))
  println(countCharsV2(testStr))

}
