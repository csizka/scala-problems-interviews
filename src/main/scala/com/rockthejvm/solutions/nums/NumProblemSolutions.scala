package com.rockthejvm.solutions.nums

import scala.annotation.tailrec
object NumberOps {
  implicit class Integer(n: Int) {
    def isPrime: Boolean = {
      val absNum = Math.abs(n)

      @tailrec
      def isPrimeHelper(index: Int): Boolean = {
        if (index == 1) true
        else if (index == 0 || absNum % index == 0) false
        else if (index > 1) isPrimeHelper(index - 1)
        else false
      }
      isPrimeHelper(Math.sqrt(absNum).toInt)
    }

    def decompose: List[Int] = {
      assert(n >= 0)
      @tailrec
      def decomposeHelper(remainder: Int, index: Int, acc: List[Int]): List[Int] = {
        if (index >= Math.sqrt(remainder)) remainder :: acc
        else if (remainder % index == 0) decomposeHelper(remainder / index, index, index :: acc)
        else decomposeHelper(remainder, index + 1, acc)
      }

      decomposeHelper(n, 2, List.empty)
    }
  }
}

object NumProblemSolutions extends App {
 import NumberOps._
  def runPrimetests(): Unit = {
    assert ((-7).isPrime)
    assert (!0.isPrime)
    assert (97.isPrime)
    assert (95.isPrime)
    assert ((-95).isPrime)
    assert ((- 97).isPrime)
    assert ((- 29).isPrime)
  }

  def decomposeTests(): Unit = {
    assert(10.decompose  == List(5, 2))
    assert(3.decompose == List(3))
    assert(14.decompose == List(7, 2))
  }
  decomposeTests()
}
