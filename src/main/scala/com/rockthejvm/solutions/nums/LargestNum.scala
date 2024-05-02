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

    def elongate(curList: List[Int], size: Int): List[Int] = {
      val curSize = curList.size
      val repeatTime = {
        if (size % curSize == 0) size / curSize
        else (size / curSize) + 1
      }
      List.fill(repeatTime)(curList).flatten
    }
    @tailrec
    def isFstBigger(tuples: List[(Int, Int)]): Boolean = tuples match {
      case Nil => false
      case head :: rest =>
        val fstHead = head._1
        val sndHead = head._2
        if (fstHead == sndHead) isFstBigger(rest)
        else if (fstHead > sndHead) true
        else false
    }

    def zipEqualSize(fst: List[Int], snd: List[Int]): List[(Int, Int)] = {
      val fstSize = fst.size
      val sndSize = snd.size
      val sizeDiff = fstSize - sndSize
      if (sizeDiff == 0) fst.zip(snd)
      else if (sizeDiff > 0) fst.zip(elongate(snd,fstSize))
      else elongate(fst, sndSize).zip(snd)
    }
    def fstToStartWith(fst:List[Int], snd: List[Int]): Boolean = {
      isFstBigger(zipEqualSize(fst, snd))
    }
    val ordering = Ordering.fromLessThan(fstToStartWith)
    @tailrec
    def makeNum(list: List[Int], acc: Int): Long = list match{
      case head :: rest => makeNum(rest, acc * 10 + head)
      case Nil => acc
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
  println()
}
