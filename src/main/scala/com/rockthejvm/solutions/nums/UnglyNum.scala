package com.rockthejvm.solutions.nums

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UnglyNum extends App {
  // ugly = only the factors 2, 3 and 5
  // 1 is ugly
  // assume positive inputs
  // examples: 6, 25, 100
  // not ugly: 14, 39
  val uglySet = Set(2,3,5)
  def uglyNum(number: Int): Boolean = {

    @tailrec
    def checkIfUglyTailrec(num: Int, divisor: Int): Boolean = {
      if (num == 1) true
      else if (num % divisor == 0 && uglySet.contains(divisor)) checkIfUglyTailrec(num / divisor, divisor)
      else if (num % divisor == 0) false
      else checkIfUglyTailrec(num, divisor + 1)
    }
    checkIfUglyTailrec(number, 2)
  }

  def nthUglyNum(n: Int): Int = {
    @tailrec
    def nthUglyTailrec(curUgly: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int], count: Int): Int = {
      if (n == count) curUgly
      else {
        val min = List(q2.head, q3.head, q5.head).min
        val newQ2 = {
          val intermedQ2 = if (min == q2.head) q2.tail else q2
          intermedQ2.enqueue(min * 2)
        }
        val newQ3 = {
          val intermedQ3 = if (min == q3.head) q3.tail else q3
          intermedQ3.enqueue(min * 3)
        }
        val newQ5 = {
          val intermedQ5 = if (min == q5.head) q5.tail else q5
            intermedQ5.enqueue(min * 5)
        }
        nthUglyTailrec(min, newQ2, newQ3, newQ5, count + 1)
      }
    }
    nthUglyTailrec(1, Queue(2), Queue(3), Queue(5), 1)
  }

  assert(uglyNum(1))
  assert(!uglyNum(49))
  assert(!uglyNum(22))
  assert(!uglyNum(14))
  assert(uglyNum(6))
  assert(uglyNum(24))
  assert(uglyNum(100))
  assert(!uglyNum(7))
  assert(!uglyNum(101))
  assert(nthUglyNum(1) == 1)
  assert(nthUglyNum(2) == 2)
  assert(nthUglyNum(10) == 12)
  assert(nthUglyNum(12) == 16)

}
