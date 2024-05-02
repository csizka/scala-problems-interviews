package com.rockthejvm.solutions.nums

import scala.annotation.tailrec


object LargestNum extends App {
  @tailrec
  def numLister(num: Int, acc: List[Int]): List[Int] = {
    if (num == 0 && !acc.isEmpty) acc
    else if (num == 0 && acc.isEmpty) List(0)
    else numLister(num / 10, num % 10 +: acc)
  }
  def largestNum(numList: List[Int]): String = {
    val listedInts = numList.map(numLister(_, List()))

    /**
     * Elongates a list by repeating its elems until reaching the requested size.
     * example: elongate(List(1,2,3), 7) = List(1,2,3,1,2,3,1,2,3)
     */
    def elongate[T](curList: List[T], size: Int): List[T] = {
      val curSize = curList.size
      val repeatTime = {
        if (size % curSize == 0) size / curSize
        else (size / curSize) + 1
      }
      List.fill(repeatTime)(curList).flatten
    }

    @tailrec
    def isFstBigger(tuples: List[(Int, Int)]): Boolean = tuples match {
      case (fstHead, sndHead) :: rest =>
        if (fstHead == sndHead) isFstBigger(rest)
        else fstHead > sndHead
      case Nil => false
    }

    /**
     * Zips 2 lists together, same as zip, but if one of the lists is shorter,
     * it repeats its elements until having the same length as the other list.
     * example: fst = List(1,2,3) snd = List(4,5,6)
     * return value = List((1,4),(2,5),(3,6))
     */
    def zipWRepeatingElems[T, S](lhs: List[T], rhs: List[S]): List[(T, S)] = {
      val lhsSize = lhs.size
      val rhsSize = rhs.size
      val sizeDiff = lhsSize - rhsSize
      if (sizeDiff == 0) lhs.zip(rhs)
      else if (sizeDiff > 0) lhs.zip(elongate(rhs,lhsSize))
      else elongate(lhs, rhsSize).zip(rhs)
    }

    val ordering = Ordering.fromLessThan[List[Int]] { (lhs, rhs) =>
      isFstBigger(zipWRepeatingElems(lhs, rhs))
    }
    listedInts.sorted(ordering).flatten.mkString
  }

  def largestNumV2(list: List[Int]): String = {
    implicit val ordering: Ordering[Int] = Ordering.fromLessThan { (a,b) =>
      val aString = a.toString
      val bString = b.toString

      (aString + bString).compareTo(bString + aString) >= 0
    }
    val largest = list.sorted.mkString
    if (largest.charAt(0) == 0) "0"
    else largest
  }
  assert(List(4, 57, 0, 68,7).map(numLister(_, List.empty)) == List(List(4), List(5, 7), List(0), List(6, 8), List(7)))
  assert(List(0, 836, 1).map(numLister(_, List.empty)) == List(List(0), List(8, 3, 6), List(1)))
  assert(largestNum(List(4, 57, 0, 68, 7, 571, 572)) == "7685757257140")
}
