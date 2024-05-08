package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object MultiplyStrigs extends App {
  @tailrec
  def strToIntListAbs(rest: String, acc: List[Int] = List.empty): List[Int] = {
    if (rest.isEmpty) acc
    else if ((1 to 9).toSet.contains(rest.head - 48)) strToIntListAbs(rest.tail, acc :+ (rest.head - 48))
    else if (rest.head == '0' && !acc.isEmpty) strToIntListAbs(rest.tail, acc :+ 0)
    else if (rest.head == '0' && acc.isEmpty) strToIntListAbs(rest.tail, acc)
    else if (rest.head == '+' && acc.isEmpty) strToIntListAbs(rest.tail, acc)
    else if (rest.head == '-' && acc.isEmpty) strToIntListAbs(rest.tail, acc)
    else if (rest.head == ' ' || rest.head == ',') strToIntListAbs(rest.tail, acc)
    else List.empty
  }
  @tailrec
  def checkSign(rest: String): Int = {
    if (rest.isEmpty) 0
    else if ((1 to 9).toSet.contains(rest.head - 48) || rest.head == '+') 1
    else if (rest.head == '-') - 1
    else if (rest.head == ' ') checkSign(rest.tail)
    else 0
  }
  @tailrec
  def numToIntList(rest: Int, acc: List[Int] = List.empty): List[Int] = {
    if (rest > 0) numToIntList(rest/10, (rest % 10) +: acc)
    else acc
  }

  @tailrec
  def add(lhsNum: List[Int], rhsNum: List[Int], carryOver: Int = 0, acc: List[Int] = List.empty): List[Int] = {
    if (lhsNum.isEmpty && rhsNum.isEmpty && carryOver == 0) acc
    else if (lhsNum.isEmpty && rhsNum.isEmpty) carryOver +: acc
    else if (lhsNum.isEmpty && carryOver == 0) rhsNum ++ acc
    else if (rhsNum.isEmpty && carryOver == 0) lhsNum ++ acc
    else if (lhsNum.isEmpty) add(lhsNum, rhsNum.dropRight(1), (rhsNum.last + carryOver) / 10, (rhsNum.last + carryOver) % 10 +: acc)
    else if (rhsNum.isEmpty) add(lhsNum.dropRight(1), rhsNum, (lhsNum.last + carryOver) / 10, (lhsNum.last + carryOver) % 10 +: acc)
    else {
      val nextAdded = lhsNum.last + rhsNum.last + carryOver
      add(lhsNum.dropRight(1), rhsNum.dropRight(1), nextAdded / 10, nextAdded % 10 +: acc)
    }
  }

  def multipyStrings(str1: String, str2: String): String = {
    @tailrec
    def multiplyTailrec(lhsNum: List[Int], rhsNum: List[Int], lhsUsed: List[Int] = List.empty, lhsDecimals: Int = 0, rhsDecimals: Int = 0, acc: List[Int] = List.empty): List[Int] = {
      if (!lhsNum.isEmpty) {
        val curLhsDigit = lhsNum.last
        val curRhsDigit = rhsNum.last
        val curRes = numToIntList(curRhsDigit * curLhsDigit) ++ List.fill(lhsDecimals + rhsDecimals)(0)
        multiplyTailrec(lhsNum.dropRight(1), rhsNum, curLhsDigit +: lhsUsed, lhsDecimals + 1, rhsDecimals, add(acc, curRes))
      } else if (lhsNum.isEmpty && !rhsNum.dropRight(1).isEmpty) multiplyTailrec(lhsUsed, rhsNum.dropRight(1), List.empty, 0, rhsDecimals + 1, acc)
      else acc
    }

    val lhsChars = strToIntListAbs(str1)
    val rhsChars = strToIntListAbs(str2)
    val sign = checkSign(str1) * checkSign(str2)
    if (sign > 0 ) multiplyTailrec(lhsChars, rhsChars).mkString("")
    else if (sign == 0) "0"
    else "-" + multiplyTailrec(lhsChars, rhsChars).mkString("")
   }

  val testList1 = List(1,1,1,1)
  val testList2 = List(2,2,2)
  val testList3 = List(7,0,2,1,7,6,2,4,3)
  val testList4 = List(8,7,3,5,2,2,3,6)
  assert(strToIntListAbs("5543") == List(5,5,4,3))
  assert(strToIntListAbs("0123") == List(1,2,3))
  assert(strToIntListAbs(" + 99") == List(9,9))
  assert(numToIntList(5543) == List(5,5,4,3))
  assert(add(testList1, testList2) == List(1,3,3,3))
  assert(add(testList4, testList3) == numToIntList(702176243 + 87352236))
  assert(add(testList3,testList2) == List(7,0,2,1,7,6,4,6,5))
  assert(add(List(2,0,0),List(2,0)) == List(2,2,0))
  assert(strToIntListAbs(" +  006,524") == List(6, 5, 2, 4))
  assert(multipyStrings("02a0", "010") == "0")
  assert(multipyStrings("2", "2") == "4")
  assert(multipyStrings("5","5") == "25")
  assert(multipyStrings("9", "9") == "81")
  assert(multipyStrings("-12", "1") == "-12")
  assert(multipyStrings("55", "+20") == "1100")
  assert(multipyStrings(" - 0 2,0", "-010") == "200")


}
