package com.rockthejvm.solutions.nums

import scala.annotation.tailrec
import scala.util.Random

object Duplicates extends App {

  def duplicates(lst: List[Int]): Int = {
    @tailrec
    def filterTailrec(list: List[Int], acc: Set[Int]): Int = list match {
      case head :: rest =>
        if (acc.contains(head)) filterTailrec(rest, acc - head)
        else filterTailrec(rest, acc + head)
      case Nil => acc.head
    }
    filterTailrec(lst, Set.empty)
  }
  def duplicatesV2(lst: List[Int]): Int = {
    val sortedList = lst.sorted
    @tailrec
    def sorter(list: List[Int]): Int = list match {
      case fstHead :: sndHead :: rest =>
        if (fstHead == sndHead) sorter(rest)
        else fstHead
      case lastHead :: Nil => lastHead
    }
    sorter(sortedList)
  }
  def duplicatesV3(lst: List[Int]): Int = {
    @tailrec
    def halverTailrec(rest: List[Int], num: Int, smaller: (List[Int], Int), bigger: (List[Int], Int)): Int = rest match {
      case head :: tail =>
        if (head < num) halverTailrec(tail, num, (head +: smaller._1, smaller._2 + 1), bigger)
        else if (head > num) halverTailrec(tail, num, smaller, (head +: bigger._1, bigger._2 + 1))
        else halverTailrec(tail, num, smaller, bigger)
      case Nil =>
        if (smaller._2 == 1) smaller._1.head
        else if (smaller._2 % 2 != 0) halverTailrec(smaller._1.tail, smaller._1.head, (List.empty, 0), (List.empty, 0))
        else if (bigger._2 == 1) bigger._1.head
        else if (bigger._2 % 2 != 0) halverTailrec(bigger._1.tail, bigger._1.head, (List.empty, 0), (List.empty, 0))
        else num
    }
    halverTailrec(lst.tail, lst.head, (List.empty, 0), (List.empty,0))
  }
  def duplicatesOptial(lst: List[Int]): Int = lst.foldLeft(0)(_^_)

  val testList: List[Int] = (534 to 7328643).toList ++ (534 to 7328643).toList :+ 7328644
  val v1Time0 = System.currentTimeMillis()
  duplicates(testList)
  val v1Time1 = System.currentTimeMillis()
  println(v1Time1-v1Time0)
  val v2Time0 = System.currentTimeMillis()
  duplicatesV2(testList)
  val v2Time1 = System.currentTimeMillis()
  println(v2Time1 - v2Time0)
  val v3Time0 = System.currentTimeMillis()
  duplicatesV3(testList)
  val v3Time1 = System.currentTimeMillis()
  println(v3Time1 - v3Time0)
  val optTime0 = System.currentTimeMillis()
  duplicatesOptial(testList)
  val optTime1 = System.currentTimeMillis()
  println(optTime1 - optTime0)
  val rand = new Random()
  val randomList = (1 to 1000000).toList.map(_ => rand.nextInt(100))
  val testList2 = randomList.map(_ * 100 + 5) ++ (randomList :+ 101) ++ randomList ++ randomList.map(_ * 100 + 5)
  val v1Time2 = System.currentTimeMillis()
  duplicates(testList2)
  val v1Time3 = System.currentTimeMillis()
  println(v1Time3 - v1Time2)
  val v2Time2 = System.currentTimeMillis()
  duplicatesV2(testList2)
  val v2Time3 = System.currentTimeMillis()
  println(v2Time3 - v2Time2)
  val v3Time2 = System.currentTimeMillis()
  duplicatesV3(testList2)
  val v3Time3 = System.currentTimeMillis()
  println(v3Time3 - v3Time2)
  val optTime2 = System.currentTimeMillis()
  duplicatesOptial(testList2)
  val optTime3 = System.currentTimeMillis()
  println(optTime3 - optTime2)
}
