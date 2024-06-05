package com.rockthejvm.solutions.graphs

object FindTownJudge extends App {

  /*
      n people, 1 to n
      trust = List[(a, b)]
      (a,b) = a trusts b

      There might be a town judge.
        The town judge trusts nobody. == outDegree(tj) = 0
        Everybody (except for the town judge) trusts the town judge. == inDegree(tj) = n-1
        There is exactly one person that satisfies these properties.

      Find the town judge, or return -1.
     */
  def findJudge(pplCount: Int, edges: List[(Int,Int)]): Int = {
    val judge = (1 to pplCount).toList.filter(curPerson =>
      !edges.exists { case (truster, trustee) => truster == curPerson } &&
        ((1 to pplCount).toSet - curPerson).forall(curOtherPerson => edges.contains((curOtherPerson, curPerson)))
      ) :+ -1
    judge.head
  }

  def indegreeOf(node: Int, edgeList: List[(Int, Int)]): Int = {
    edgeList.foldLeft(0) { case (curCount, (curTruster, curTrustee)) => if (curTrustee == node) curCount + 1 else curCount
    }
  }

  def outdegreeOf(node: Int, edges: List[(Int, Int)]): Int = {
    edges.foldLeft(0){case (curCount, (curTruster, curTrustee)) => if (curTruster == node) curCount + 1 else curCount}
  }

  def findJudgeV2(nodeCount: Int, edges:List[(Int, Int)]): Int = {
    val judge = (1 to nodeCount).find(elem => indegreeOf(elem, edges) == nodeCount - 1 && outdegreeOf(elem, edges) == 0)
    judge.getOrElse(-1)
  }
}
