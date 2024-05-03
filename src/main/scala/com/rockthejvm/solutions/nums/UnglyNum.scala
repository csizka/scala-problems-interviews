package com.rockthejvm.solutions.nums

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UnglyNum extends App {

  val uglySet = Set(2,3,5)
  def uglyNum(number: Int): Boolean = {

    @tailrec
    def uglyChechkerRec(num: Int, divisor: Int): Boolean = {
      if (num == 1) true
      else if (num % divisor == 0 && uglySet.contains(divisor)) uglyChechkerRec(num / divisor, divisor)
      else if (num % divisor == 0) false
      else uglyChechkerRec(num, divisor + 1)
    }
    uglyChechkerRec(number, 2)
  }

  def nthUglyNum(n: Int): Int = {
    @tailrec
    def uglyNthRecur(num: Int, queue: List[Set[Int]], count: Int): Int = {
      if (count == n) num
      else {
        val nextMin = queue.map(x => x.min).min
        uglyNthRecur(nextMin, List(2,3,5).map(x => queue((x / 2 + x % 2) - 1) - nextMin + x * nextMin), count + 1)
      }
    }
    uglyNthRecur(1, List(Set(2), Set(3), Set(5)), 1)
  }

  def nthUgly(n: Int): Int = {
    @tailrec
    def nthUglyTailrec(num: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int], count: Int): Int = {
      if (n == count) num
      else {
        val min = List(q2.head, q3.head, q5.head).min
        val newQ2 = {
          (if (min == q2.head)  q2.tail
          else q2).enqueue(min * 2)
        }
        val newQ3 = {
          (if (min == q3.head) q3.tail
          else q3).enqueue(min * 3)
        }
        val newQ5 = {
          (if (min == q5.head) q5.tail
          else q5).enqueue(min * 5)
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
  assert(nthUgly(10) == nthUglyNum(10))
  assert(nthUgly(85) == nthUglyNum(85))
  assert(nthUgly(41) == nthUglyNum(41))
//  val fstTime0 = System.currentTimeMillis()
//  nthUglyNum(1578797)
//  val fstTime1 = System.currentTimeMillis()
//  println(fstTime1-fstTime0)
//  val sndTime0 = System.currentTimeMillis()
//  nthUgly(1578797)
//  val sndTime1 = System.currentTimeMillis()
//  println(sndTime1 - sndTime0)

}
