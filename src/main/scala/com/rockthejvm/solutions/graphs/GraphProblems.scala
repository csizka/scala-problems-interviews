package com.rockthejvm.solutions.graphs

import scala.annotation.tailrec

object GraphProblems extends App {
  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Mary", "Bob"),
    "Mary" -> Set("Bob", "Charlie")
  )
  def outDegree[T](graph: Graph[T], node: T): Int = {
    if (graph.contains(node)) graph(node).size
    else 0
  }
  def inDegree[T](graph: Graph[T], node: T): Int = graph.foldLeft(0){ case (acc, (key, set)) => if (set.contains(node)) acc + 1 else acc}
  def inDegreeV2[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

  def isPath[T](graph: Graph[T], start: T, target: T): Boolean = {
    @tailrec
    def findPathTailRec(curLevel: Set[T], visited: Set[T]): Boolean =
      if (curLevel.contains(target)) true
      else {
        val nextLevel = curLevel.flatMap(graph(_)) -- visited
        if (nextLevel.isEmpty) false
        else findPathTailRec(nextLevel, visited ++ nextLevel)
      }
    findPathTailRec(graph(start), graph(start))
  }

  assert(outDegree(socialNetwork, "Alice") == 3)
  assert(outDegree(socialNetwork, "Bob") == 0)
  assert(outDegree(socialNetwork, "David") == 2)
  assert(inDegree(socialNetwork, "Alice") == 0)
  assert(inDegree(socialNetwork, "Bob") == 3)
  assert(inDegree(socialNetwork, "David") == 2)
  assert(inDegreeV2(socialNetwork, "Alice") == 0)
  assert(inDegreeV2(socialNetwork, "Bob") == 3)
  assert(inDegreeV2(socialNetwork, "David") == 2)
  assert(isPath(socialNetwork, "Alice", "Bob"))
  assert(isPath(socialNetwork, "David", "Charlie"))
  assert(!isPath(socialNetwork, "Charlie", "Alice"))
}
