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

  def findPath[T](graph: Graph[T], start: T, target: T): List[T] = {
    @tailrec
    def findTailrec(toCheck: List[List[T]], checked: Set[T]): List[T] = {
      if (toCheck.isEmpty) List()
      else if (toCheck.head.head == target) toCheck.head.reverse
      else if (checked.contains(toCheck.head.head)) findTailrec(toCheck.tail, checked)
      else {
        val newElems = graph(toCheck.head.head) -- checked
        val nextCheckList = newElems.map(nextElem => nextElem :: toCheck.head).toList
        findTailrec(toCheck.tail ++ nextCheckList, checked + toCheck.head.head)
      }
    }
    @tailrec
    def findPathTailrecV2(toCheck: List[List[T]], checked: Set[T]): List[T] = {
      if (toCheck.isEmpty) List()
      else {
        val rightPaths = toCheck.filter(_.head == target)
        if (!rightPaths.isEmpty) rightPaths.head.reverse
        else {
          val nextLevel = toCheck.flatMap(elemList => graph(elemList.head).filter(!checked.contains(_)).toList.map(_ :: elemList))
          val newChecked = nextLevel.map(elems => elems.head).toSet ++ checked
          findPathTailrecV2(nextLevel, newChecked)
        }
      }
    }
    findTailrec(List(List(start)), Set.empty)
    findPathTailrecV2(List(List(start)), Set.empty)
  }
  def findCycle[T](graph: Graph[T], node:T): List[T] = {
    @tailrec
    def findTailrec(toCheck: List[List[T]]): List[T] = {
      if (toCheck.isEmpty) List()
      else if (toCheck.head.isEmpty) findTailrec(toCheck.tail)
      else if (toCheck.head.tail.toSet.contains(toCheck.head.head)) {
        val cycleIndex = toCheck.head.tail.indices.filter(toCheck.head.tail(_) == toCheck.head.head).head
        toCheck.head.tail.take(cycleIndex + 1).reverse
      } else {
        val nextCheckList = graph(toCheck.head.head).toList.map(elem => elem :: toCheck.head)
        findTailrec(toCheck.tail ++ nextCheckList)
      }
    }
    findTailrec(List(List(node)))
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
  assert(findPath(socialNetwork, "Alice", "Bob") == List("Alice", "Bob"))
  assert(findPath(socialNetwork, "David", "Charlie") == List("David", "Mary", "Charlie"))
  assert(findPath(socialNetwork, "Charlie", "Alice").isEmpty)
  assert(!findCycle(socialNetwork, "Alice").isEmpty)
  assert(!findCycle(socialNetwork, "David").isEmpty)
  assert(findCycle(socialNetwork, "Bob").isEmpty)
}
