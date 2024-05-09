package com.rockthejvm.solutions.strings

object ReverseWords extends App {
  def reorgWords(str: String): String = str.split("\\s+").reverse.mkString(" ")
  def reorgWordsV2(str:String): String = str.split(" ").filter(_ != "").reverse.mkString(" ")

  val nums = "   One Two   Three     Four Five Six Seven Eight Nine Ten  ".repeat(1000000)

  val v1ZeroTime = System.currentTimeMillis()
  reorgWords(nums)
  val v1EndTime = System.currentTimeMillis()
  println(v1EndTime-v1ZeroTime)
  val v2ZeroTime = System.currentTimeMillis()
  reorgWordsV2(nums)
  val v2EndTime = System.currentTimeMillis()
  println(v2EndTime - v2ZeroTime)

}
