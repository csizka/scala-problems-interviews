package com.rockthejvm.solutions.strings

object Anagrams extends App {

  def charCounter(str: String): Map[Char, Int] =
    str.foldLeft(Map.empty: Map[Char, Int])((map, char: Char) => map + (char -> (map.getOrElse(char, 0) + 1)))
  def areAnagrams(lhsString: String, rhsString: String): Boolean = {
    val lhsCount = charCounter(lhsString)
    val rhsCount = charCounter(rhsString)
    lhsCount == rhsCount
  }
  def areAnagramsV2(lhsString: String, rhsString: String): Boolean = {
    lhsString.sorted == rhsString.sorted
  }

  val testStr11 = "desserts"
  val testStr12 = "stressed"
  val notAnag = "dessert"
  val testStr21 = "kjsvfk sdfk6723 638"
  val testStr22 = "jsvfk sfk723d83 k66"
  val testStr23 = "svfk sfkf723d83 k66"
  assert(areAnagrams(testStr11, testStr12))
  assert(!areAnagrams(notAnag, testStr12))
  assert(areAnagrams(testStr22, testStr21))
  assert(!areAnagrams(testStr22, testStr23))
  assert(areAnagramsV2(testStr11, testStr12))
  assert(!areAnagramsV2(notAnag, testStr12))
  assert(areAnagramsV2(testStr22, testStr21))
  assert(!areAnagramsV2(testStr22, testStr23))
}
