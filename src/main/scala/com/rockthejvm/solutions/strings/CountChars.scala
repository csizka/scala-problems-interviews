package com.rockthejvm.solutions.strings
import scala.annotation.tailrec

object CountChars extends App {

  def countChars(str: String): Map[Char, Int] = {
    str.foldLeft(Map.empty: Map[Char, Int]) {case (curMap, curChar) => curMap + (curChar -> (curMap.getOrElse(curChar, 0) + 1))}
  }
  val testStr = "random checker 1 aaa a 2 1"
  println(countChars(testStr))

}
