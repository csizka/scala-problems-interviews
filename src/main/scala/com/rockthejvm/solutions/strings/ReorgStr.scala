package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object ReorgStr {

  // rearrange chars so that no two adjacent chars are identical
  def reorgStr(ogString: String): Either[String,List[Char]] = {
    @tailrec
    def reorg(restCharCounts: Map[Char, Int], curRes: List[Char], forbiddenChar: Char = '\u0000'): List[Char] = {
      if (restCharCounts.isEmpty) curRes
      else {
        val (curChar, curCount) = (restCharCounts - forbiddenChar).maxBy{ case (x, y) => y}
        val newCharCount =
          if (curCount > 1) restCharCounts + (curChar -> (curCount - 1))
          else restCharCounts - curChar
        reorg(newCharCount, curChar :: curRes, curChar)
      }
    }
    val length = ogString.length
    val mappedStr = ogString.groupBy(identity).view.mapValues(_.length).toMap
    val possible = !mappedStr.values.exists(_ > (length + 1) / 2)
    if (!possible) Left(s"Too many repetitions in the input:'$ogString', not possible to reorg. :(")
    else Right(reorg(mappedStr, List()))
  }

  def main(args: Array[String]): Unit = {

//    println(reorgStr("aaaa"))
    val testMap = "aaaaaa".groupBy(identity).view.mapValues(_.length).toMap
    val testsize = 6
    assert(testMap == Map('a' -> 6))
    assert(testMap.exists(x => x._2 > testsize / 2 + 1))
    assert(reorgStr("aaab") == Left("Too many repetitions in the input:'aaab', not possible to reorg. :("))
    assert(reorgStr("aaaa") == Left("Too many repetitions in the input:'aaaa', not possible to reorg. :("))
    assert(reorgStr("aabb") == Right(List('a','b','a','b')) || reorgStr("aabb") ==  Right(List('b','a','b', 'a')))
    assert(reorgStr("aaabc") == Right(List('a','b','a','c','a')) || reorgStr("aaabc") == Right(List('a','c','a','b','a')))
    println(reorgStr("515155323"))
    println(reorgStr("nsjlzzzzzaazz"))

  }
}
