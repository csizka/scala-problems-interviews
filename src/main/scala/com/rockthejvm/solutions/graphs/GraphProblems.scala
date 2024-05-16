package com.rockthejvm.solutions.graphs

import scala.annotation.tailrec

object GraphProblems extends App {
  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David", "Lulu"),
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
    val keyset = graph.keySet
    @tailrec
    def findPathTailRec(curLevel: Set[T], visited: Set[T]): Boolean =
      if (curLevel.contains(target)) true
      else {
        val nextLevel = (curLevel.flatMap(graph(_)) -- visited) & keyset
        if (nextLevel.isEmpty) false
        else findPathTailRec(nextLevel, visited ++ nextLevel)
      }
    findPathTailRec(graph(start), graph(start))
  }

  def findPath[T](graph: Graph[T], start: T, target: T): List[T] = {
    val keySet: Set[T] = graph.keySet
    @tailrec
    def findTailrec(toCheck: List[List[T]], checked: Set[T]): List[T] = {
      if (toCheck.isEmpty) List()
      else if (toCheck.head.head == target) toCheck.head.reverse
      else if (checked.contains(toCheck.head.head) || !keySet.contains(toCheck.head.head)) findTailrec(toCheck.tail, checked)
      else {
        val newElems = graph(toCheck.head.head) -- checked
        val nextCheckList = newElems.map(nextElem => nextElem :: toCheck.head).toList
        findTailrec(toCheck.tail ++ nextCheckList, checked + toCheck.head.head)
      }
    }
    val nullSet: Set[T] = Set()
    val notKeys = graph.foldLeft(nullSet) { case (acc, elem -> set) => set ++ acc} -- keySet
    @tailrec
    def findPathTailrecV2(toCheck: List[List[T]], checked: Set[T]): List[T] = {
      if (toCheck.isEmpty) List()
      else {
        val rightPaths = toCheck.filter(_.head == target)
        if (!rightPaths.isEmpty) rightPaths.head.reverse
        else {
          val filteredPaths = toCheck.filter(list => !checked.contains(list.head)).filter(list => !notKeys.contains(list.head))
          val nextLevel = filteredPaths.flatMap(elemList => graph(elemList.head).toList.map(_ :: elemList))
          val nextChecked = toCheck.map(elems => elems.head).toSet ++ checked
          findPathTailrecV2(nextLevel, nextChecked)
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
      else if (toCheck.head.isEmpty || !graph.contains(toCheck.head.head)) findTailrec(toCheck.tail)
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

  def makeUndirected[T](graph:Graph[T]): Graph[T] = {
    @tailrec
    def unDirectedTailrec(restNodes: List[T], acc: Graph[T]): Graph[T] = {
      if (restNodes.isEmpty) acc
      else {
        val node = restNodes.head
        val existingFriends = acc(node) & acc.keySet
        val newFriends = acc(node) &~ existingFriends
        val newGraph = acc.map{ case (curNode, curSet) =>
          if (existingFriends.contains(curNode)) curNode -> (acc(curNode) + node)
          else curNode -> acc(curNode)}
        if (newFriends.isEmpty) unDirectedTailrec(restNodes.tail, newGraph)
        else {
          @tailrec
          def addNewPplToNetwork(set: Set[T], accumulator: Graph[T]): Graph[T] = {
            if (set.isEmpty) accumulator
            else {
              val newPerson = set.head
              addNewPplToNetwork(set.tail, accumulator + (newPerson -> Set(node)))
            }
          }
          val finalGraph = addNewPplToNetwork(newFriends, newGraph)
          unDirectedTailrec(restNodes.tail, finalGraph)
        }
      }
    }
    val nodeList = graph.keySet.toList
    unDirectedTailrec(nodeList, graph)
  }

  def colourGraph[T](graph: Graph[T]): Map[T, Int] = {
    val correctedGraph = makeUndirected(graph)
    val keySet = correctedGraph.keySet
    @tailrec
    def colourTailrec(rest: Set[T], acc: List[(Int, Set[T])]): Map[T, Int] = {
      if (rest.isEmpty) acc.flatMap{ case (colour, list) => list.map(elem => (elem, colour))}.toMap
      else {
        val head = rest.head
        val nextRes = {
          val existingColours = acc.map { case (colour, set) =>
            if (set.flatMap(elem => correctedGraph(elem)).contains(head)) (colour, set)
            else (colour, set + head)
          }
          if (existingColours != acc) existingColours
          else acc :+ (acc.size, Set(head))
        }
        colourTailrec(rest.tail, nextRes)
      }
    }
    @tailrec
    def colourTailrecV2(rest: Set[T], acc: List[(Int, Set[T])]): Map[T,Int] = {
      if (rest.isEmpty) acc.flatMap{ case (int, set) => set.map(elem => (elem, int))}.toMap
      else {
        val head = rest.head
        val headNeighbours = correctedGraph(head)
        val nextRes = {
          val indexes = acc.indices.find(index => (acc(index)._2 & headNeighbours).isEmpty)
          if (!indexes.isEmpty) acc.updated(indexes.get, (indexes.get, acc(indexes.get)._2 + head))
          else acc :+ (acc.size, Set(head))
        }
        colourTailrecV2(rest.tail, nextRes)
      }
    }

    @tailrec
    def colourTailrecV3(rest: Set[T], colour: Int, acc: Map[T, Int]): Map[T,Int] = {
      if (rest.isEmpty) acc
      else {
        val head = rest.head
        val nextColour = {
          rest.tail.foldLeft(Map(head -> colour))((intermediateMap, elem) =>
            if (!intermediateMap.keySet.flatMap(key => correctedGraph(key)).contains(elem))
              intermediateMap + (elem -> colour)
            else intermediateMap)
        }
            colourTailrecV3(rest -- nextColour.keySet, colour + 1, acc ++ nextColour)
      }
    }
    colourTailrec(keySet, List())
    colourTailrecV2(keySet, List())
    colourTailrecV3(keySet, 0, Map.empty)
  }

  assert(outDegree(socialNetwork, "Alice") == 4)
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
  println(makeUndirected(socialNetwork))
  println(colourGraph(socialNetwork))
}
