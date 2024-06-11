package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object CompareVesionNums extends App {
  def parseVersionNums(str: String): List[Int] =
    str.split(".").toList.map(x => x.toInt)
  // compare 2 version nums and return 1 if the 2nd is newer, return 0 if they are the same, return -1 is the first is newer
  def compareVersionNums(ver1: String, ver2: String): Int = {
    val versionList1 = parseVersionNums(ver1)
    val versionList2 = parseVersionNums(ver2)
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

  def compareVersionNumsV2(ver1: String, ver2: String): Int = {
    val parsedVersion1 = parseVersionNums(ver1)
    val parsedVersion2 = parseVersionNums(ver2)
    val sizediff = parsedVersion1.size - parsedVersion2.size
    val diffToAdd = List.fill(Math.abs(sizediff))(0)
    val (v1ToComapre, v2ToCompare) = {
      if (sizediff > 0) (parsedVersion1, parsedVersion2 ++ diffToAdd)
      else if (sizediff < 0) (parsedVersion1 ++ diffToAdd, parsedVersion2)
      else (parsedVersion1, parsedVersion2)
    }
    val zippedVersions = v1ToComapre.zip(v2ToCompare)

    zippedVersions.foldLeft(0){ case (prevDiff, (curLeft, curRight)) =>
      val curDiff = curRight - curLeft
    if (prevDiff > 0 || curDiff > 0) 1
    else if (prevDiff < 0 || curDiff < 0) -1
    else 0
    }
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
  assert(compareVersionNums(testV1, testV2) == 0)
  assert(compareVersionNums(testV1, testV3) == -1)
  assert(compareVersionNums(testV4, testV2) == 1)
  assert(compareVersionNums(testV5, testV6) == 1)
  assert(compareVersionNums(testV1, testV5) == -1)
  assert(compareVersionNums(testV1, testV7) == 1)
  assert(compareVersionNumsV2(testV1, testV2) == 0)
  assert(compareVersionNumsV2(testV1, testV3) == -1)
  assert(compareVersionNumsV2(testV4, testV2) == 1)
  assert(compareVersionNumsV2(testV5, testV6) == 1)
  assert(compareVersionNumsV2(testV1, testV5) == -1)
  assert(compareVersionNumsV2(testV1, testV7) == 1)
  assert(compareVersionNumsV2(testV1, testV2) == 0)
  assert(compareVersionNumsV2(testV1, testV3) == -1)
  assert(compareVersionNumsV2(testV4, testV2) == 1)
  assert(compareVersionNumsV2(testV5, testV6) == 1)
  assert(compareVersionNumsV2(testV1, testV5) == -1)
  assert(compareVersionNumsV2(testV1, testV7) == 1)
}
