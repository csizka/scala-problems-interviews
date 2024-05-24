package com.rockthejvm.solutions.graphs

import scala.annotation.tailrec

object NetworkDelays extends App {
  def computeNetworkDelay(n: Int, network: List[(Int, Int, Int)], source: Int): Int = {
    val emptyRoutes = (1 to n).foldLeft(Map.empty: Map[Int, Set[(Int, Int)]])((acc, num) => acc + (num -> Set()))
    val routes = network.foldLeft(emptyRoutes){ case ( acc, (from, to, time)) => acc + (from -> (acc.getOrElse(from, Set()) + ((to, time))))}

    @tailrec
    def fastestRoutes(curRoutes: Set[(Int, Int, List[Int])]): Set[(Int, Int, List[Int])] = {
      val nextPosRoutes = curRoutes.flatMap{ case (end, time, path) => routes(end).map{ case (nextEnd, lastTime) => (nextEnd, time + lastTime, end :: path)}}
      val nextRoutes = (nextPosRoutes ++ curRoutes).foldLeft(Set.empty: Set[(Int, Int, List[Int])]){ case (acc, (curEnd, curTime, curPath)) =>
        val existingEnd: Option[(Int, Int, List[Int])] = acc.find { case (end, time, path) => end == curEnd }
        if (!existingEnd.isEmpty && existingEnd.get._2 > curTime) (acc - existingEnd.get) + ((curEnd, curTime, curPath))
        else if (!existingEnd.isEmpty) acc
        else acc + ((curEnd, curTime, curPath))
      }
      if (nextRoutes == curRoutes) curRoutes
      else fastestRoutes(nextRoutes)
    }

    val foundRoutes = fastestRoutes(Set((source, 0, List())))
    val allCovered = (1 to n).forall( node => foundRoutes.exists { case (curNode, _,_) => curNode == node})
      if (allCovered) foundRoutes.foldLeft(0){ case (acc, (end, time, path)) =>
        if (acc > time) acc
        else time}
      else -1
  }
  def computeNetworkDelayV2(n: Int, network: List[(Int, Int, Int)], source: Int): Int = {
    val emptyRoutes = (1 to n).foldLeft(Map.empty: Map[Int, Set[(Int, Int)]])((acc, num) => acc + (num -> Set()))
    val routes = network.foldLeft(emptyRoutes) { case (acc, (from, to, time)) => acc + (from -> (acc.getOrElse(from, Set()) + ((to, time)))) }

    @tailrec
    def fastestRoutes(restNodes: List[(Int, Int, List[Int])], acc: Set[(Int, Int, List[Int])]): Set[(Int, Int, List[Int])] = {
      if (restNodes.isEmpty) acc
      else {
        val (curNode, curTime, curPath) = restNodes.head
        val nextNodes = routes(curNode).map{ case (nextNode, addTime) => (nextNode, curTime + addTime, curNode :: curPath)}.toList
        val prevPath = acc.find{ case (node, _, _) => node == curNode}
        if (prevPath.isEmpty) fastestRoutes(nextNodes ++ restNodes.tail, acc + ((curNode, curTime, curPath)))
        else {
          val (prevNode, prevTime, prevRoute) = prevPath.get
          if (prevTime > curTime) fastestRoutes(nextNodes ++ restNodes.tail, acc + ((curNode, curTime, curPath)) - ((prevNode, prevTime, prevRoute)))
          else fastestRoutes(restNodes.tail, acc)
        }
      }
    }

    val foundRoutes = fastestRoutes(List((source, 0, List())), Set())
    val allCovered = (1 to n).forall(node => foundRoutes.exists { case (curNode, _, _) => curNode == node })
    if (allCovered) foundRoutes.maxBy{_._2}._2
    else -1
  }

val testList = List((1,2,3), (1,3,10), (1,4,10), (2,3,4), (3,4,2))
  assert(computeNetworkDelay(4, testList, 1) == 9)
  assert(computeNetworkDelay(4, testList, 2) == -1)
  assert(computeNetworkDelay(4, testList, 3) == -1)
  assert(computeNetworkDelay(4, testList, 4) == -1)
  assert(computeNetworkDelayV2(4, testList, 1) == 9)
  assert(computeNetworkDelayV2(4, testList, 2) == -1)
  assert(computeNetworkDelayV2(4, testList, 3) == -1)
  assert(computeNetworkDelayV2(4, testList, 4) == -1)

}
