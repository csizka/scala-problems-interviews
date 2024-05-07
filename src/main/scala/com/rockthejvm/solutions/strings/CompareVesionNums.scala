package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object CompareVesionNums extends App {


  def compareVersionNums(ver1: String, ver2: String): Int = {
    val sizediff = ver1.length - ver2.length
    val versionList1 =
      if (sizediff >= 0) ver1.split(".").toList.map(_.toInt)
      else ver1.split(".").toList.map(x => x.toInt) ++ List.fill(0-sizediff)(0)
    val versionList2 =
      if (sizediff <= 0) ver2.split(".").toList.map(_.toInt)
      else ver2.split(".").toList.map(_.toInt) ++ List.fill(sizediff)(0)
    val zippedLists = versionList1.zip(versionList2)
    @tailrec
    def compare(rest: List[(Int, Int)]): Int = rest match {
      case Nil => 0
      case (lhs, rhs) :: rest =>
        if (lhs == rhs) compare(rest)
        else if (lhs > rhs) 1
        else -1
    }
    compare(zippedLists)
  }
  def r(ver1: String, ver2: String): Int = {
    val versionList1 = ver1.split(".").toList.map(x => x.toInt)
    val versionList2 = ver2.split(".").toList.map(x => x.toInt)
    @tailrec
    def compareV2(lhs: List[Int], rhs: List[Int]): Int = lhs match {
      case lhsHead :: lhsRest => rhs match {
        case rhsHead :: rhsRest =>
          if (lhsHead == rhsHead) compareV2(lhsRest, rhsRest)
          else if (lhsHead > rhsHead) 1
          else -1
        case Nil =>
          if (lhs.exists( _ > 0)) 1
          else 0
      }
      case Nil => rhs match {
        case Nil => 0
        case _ =>
          if (rhs.exists(_ > 0)) -1
          else 0
      }
    }
    compareV2(versionList1, versionList2)
  }

  val testV1 = "1.00002.3"
  val testV2 = "01.2.03"
  val testV3 = "1.2.4.1.0.0"
  val testV4 = "10.2.2.1"
  val testV5 = "02.02.3"
  val testV6 = "2.001.02"
  val testV7 = "1.2"
  assert(compareVersionNums(testV1, testV2) == 0)
  assert(compareVersionNums(testV1, testV3) == -1)
  assert(compareVersionNums(testV4, testV2) == 1)
  assert(compareVersionNums(testV5, testV6) == 1)
  assert(compareVersionNums(testV1, testV5) == -1)
  assert(compareVersionNums(testV1, testV7) == 1)
  assert(r(testV1, testV2) == 0)
  assert(r(testV1, testV3) == -1)
  assert(r(testV4, testV2) == 1)
  assert(r(testV5, testV6) == 1)
  assert(r(testV1, testV5) == -1)
  assert(r(testV1, testV7) == 1)
}
