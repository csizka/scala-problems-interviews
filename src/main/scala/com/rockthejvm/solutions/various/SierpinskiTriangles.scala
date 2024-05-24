package com.rockthejvm.solutions.various

import scala.annotation.tailrec

object SierpinskiTriangles extends App {

  def sierpinski(n: Int): String = {
@tailrec
    def levelBuildRecur(acc: List[String], curLevel: Int): List[String] = {
      if (curLevel == n) acc
      else {
        val spaces = " ".repeat(1 << curLevel)
        val nextAcc  = acc.map(spaces +  _ + spaces)
        val newTriange = acc.map( row => row + " " + row)
        levelBuildRecur(nextAcc ++ newTriange, curLevel + 1)
        }
      }
    if (n > 0) levelBuildRecur(List(" * ","* *"), 1).foldLeft("")((acc, curLine) => acc + curLine + "\n")
    else ""
    }

  println(sierpinski(5))

  }

