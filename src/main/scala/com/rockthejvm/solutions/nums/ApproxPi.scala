package com.rockthejvm.solutions.nums

import scala.math.sqrt

object ApproxPi extends App {

  def approximatePi(n: Int): Double = {
    val rand = new scala.util.Random(System.currentTimeMillis())
    (0 until n).map(_ => (rand.nextDouble, rand.nextDouble)).count { case (x, y) => sqrt(x * x + y * y) <= 1.0 }* 4.0 / n
  }
  println(approximatePi(10000000))
}
