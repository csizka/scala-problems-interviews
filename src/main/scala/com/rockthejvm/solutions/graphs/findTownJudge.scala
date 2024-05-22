package com.rockthejvm.solutions.graphs

object findTownJudge extends App {

  def findJudge(n: Int, lst: List[(Int,Int)]): Int = {
    val judge = (1 to n).toList.filter(elem =>
      !lst.exists(_._1 == elem) &&
        ((1 to n).toSet - elem).forall(num => lst.contains((num, elem)))
    ) :+ -1
    judge.head
  }

  def indegree(n: Int, lst: List[(Int, Int)]): Int = {
    lst.foldLeft(0)((curCount, tuple) => if (tuple._2 == n) curCount + 1 else curCount)
  }
  def outdegree(n: Int, lst: List[(Int, Int)]): Int = {
    lst.foldLeft(0)((curCount, tuple) => if (tuple._1 == n) curCount + 1 else curCount)
  }
  def findJudheV2(n: Int, lst:List[(Int, Int)]): Int = {
    val judge = (1 to n).find(elem => indegree(elem, lst) == n - 1 && outdegree(elem, lst) == 0)
    if (!judge.isEmpty) judge.get
    else -1
  }

}
