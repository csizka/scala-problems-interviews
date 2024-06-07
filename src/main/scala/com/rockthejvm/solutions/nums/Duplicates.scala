package com.rockthejvm.solutions.nums

import scala.annotation.tailrec
import scala.util.Random

object Duplicates extends App {
  // all numbers in the list appear EXACTLY twice, EXCEPT one: find that number
  def duplicatesOneByOne(lst: List[Int]): Int = {
    @tailrec
    def filterTailrec(list: List[Int], acc: Set[Int]): Int = list match {
      case head :: rest =>
        if (acc.contains(head)) filterTailrec(rest, acc - head)
        else filterTailrec(rest, acc + head)
      case Nil => acc.head
    }
    filterTailrec(lst, Set.empty)
  }

  def duplicatesSortThenTwoByTwo(lst: List[Int]): Int = {
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
  def duplicatesHalver(lst: List[Int]): Int = {
    @tailrec
    def halverTailrec(rest: List[Int], num: Int, smaller: (List[Int], Int), bigger: (List[Int], Int)): Int = {
      val (biggerNums, biggerCount) = bigger
      val (smallerNums, smallerCount) = smaller
      rest match {
        case head :: tail =>
          if (head < num) halverTailrec(tail, num, (head +: smallerNums, smallerCount + 1), bigger)
          else if (head > num) halverTailrec(tail, num, smaller, (head +: biggerNums, biggerCount + 1))
          else halverTailrec(tail, num, smaller, bigger)
        case Nil =>
          if (smallerCount == 1) smallerNums.head
          else if (smallerCount % 2 != 0) halverTailrec(smallerNums.tail, smallerNums.head, (List.empty, 0), (List.empty, 0))
          else if (biggerCount == 1) biggerNums.head
          else if (biggerCount % 2 != 0) halverTailrec(biggerNums.tail, biggerNums.head, (List.empty, 0), (List.empty, 0))
          else num
      }
    }
    halverTailrec(lst.tail, lst.head, (List.empty, 0), (List.empty,0))
  }
  def duplicatesOptimal(lst: List[Int]): Int = lst.foldLeft(0)(_^_)

  val testList: List[Int] = (534 to 7328643).toList ++ (534 to 7328643).toList :+ 7328644
  val v1Time0 = System.currentTimeMillis()
  duplicatesOneByOne(testList)
  println("duplicatesOneByOne :")
  val v1Time1 = System.currentTimeMillis()
  println(v1Time1-v1Time0)
  val v2Time0 = System.currentTimeMillis()
  duplicatesSortThenTwoByTwo(testList)
  println("duplicatesSortThenTwoByTwo :")
  val v2Time1 = System.currentTimeMillis()
  println(v2Time1 - v2Time0)
  val v3Time0 = System.currentTimeMillis()
  duplicatesHalver(testList)
  println("duplicatesHalver :")
  val v3Time1 = System.currentTimeMillis()
  println(v3Time1 - v3Time0)
  val optTime0 = System.currentTimeMillis()
  duplicatesOptimal(testList)
  println("duplicatesOptimal :")
  val optTime1 = System.currentTimeMillis()
  println(optTime1 - optTime0)
  val rand = new Random()
  val randomList = (1 to 1000000).toList.map(_ => rand.nextInt(100))
  val testList2 = randomList.map(_ * 100 + 5) ++ (randomList :+ 101) ++ randomList ++ randomList.map(_ * 100 + 5)
  val v1Time2 = System.currentTimeMillis()
  duplicatesOneByOne(testList2)
  println("duplicatesOneByOne :")
  val v1Time3 = System.currentTimeMillis()
  println(v1Time3 - v1Time2)
  val v2Time2 = System.currentTimeMillis()
  duplicatesSortThenTwoByTwo(testList2)
  println("duplicatesSortThenTwoByTwo :")
  val v2Time3 = System.currentTimeMillis()
  println(v2Time3 - v2Time2)
  val v3Time2 = System.currentTimeMillis()
  duplicatesHalver(testList2)
  println("duplicatesHalver :")
  val v3Time3 = System.currentTimeMillis()
  println(v3Time3 - v3Time2)
  val optTime2 = System.currentTimeMillis()
  duplicatesOptimal(testList2)
  println("duplicatesOptimal :")
  val optTime3 = System.currentTimeMillis()
  println(optTime3 - optTime2)
}
