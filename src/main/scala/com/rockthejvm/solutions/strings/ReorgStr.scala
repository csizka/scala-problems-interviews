package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object ReorgStr {

  def reorgStr(str: String): String = {
    @tailrec
    def reorg(map: Map[Char, Int], acc: String = "", forbiddenChar: Char = '\u0000'): String = {
      if (map.isEmpty) acc
      else {
        val (curChar, curCount) = (map - forbiddenChar).maxBy{ case (x, y) => y}
        val newMap =
          if (curCount > 1) map + (curChar -> (curCount - 1))
          else map - curChar
        reorg(newMap, curChar + acc)
      }
      }
    val length = str.length
    val mappedStr = str.groupBy(identity).view.mapValues(_.length).toMap
    val possible = !mappedStr.values.exists(_ > (length + 1) / 2)
    if (!possible) "Too many repetitions, not possibleto reorg. :("
    else reorg(mappedStr)
  }

  def main(args: Array[String]): Unit = {

//    println(reorgStr("aaaa"))
    val testMap = "aaaaaa".groupBy(identity).view.mapValues(_.length).toMap
    val testsize = 6
    assert(testMap == Map('a' -> 6))
    assert(testMap.exists(x => x._2 > testsize / 2 + 1))
    println(reorgStr("aaab"))
    println(reorgStr("aaaa"))
    println(reorgStr("aabb"))
    println(reorgStr("aaabc"))
    println(reorgStr("515155323"))
    println(reorgStr("nsjlzzzzzaazz"))

  }
}
