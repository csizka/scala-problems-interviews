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

  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int = graph.getOrElse(node, Set()).size

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int = graph.foldLeft(0) { case (curCount, (key, set)) => if (set.contains(node)) curCount + 1 else curCount}

  def inDegreeV2[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

  //Is trere a path from start to target?
  def isPath[T](graph: Graph[T], start: T, target: T): Boolean = {
    @tailrec
    def findPathTailRec(curLevel: Set[T], visited: Set[T]): Boolean =
      if (curLevel.contains(target)) true
      else {
        val nextLevel = curLevel.flatMap(graph.getOrElse(_, Set())) -- visited
        if (nextLevel.isEmpty) false
        else findPathTailRec(nextLevel, visited ++ nextLevel)
      }
    findPathTailRec(graph.getOrElse(start, Set()) + start, graph.getOrElse(start, Set()) + start)
  }

  //Return the path from start to target, id there is no path, return List()
  def findPath[T](graph: Graph[T], start: T, target: T): List[T] = {
    @tailrec
    def findTailrec(toCheck: List[List[T]], checked: Set[T]): List[T] = toCheck match{
      case Nil => List()
      case curPath :: rest if curPath.head == target => curPath.reverse
      case curPath :: rest if checked.contains(curPath.head) => findTailrec(rest, checked)
      case curPath :: rest =>
        val newElems = graph.getOrElse(curPath.head, Set()) -- checked
        val nextCheckList = newElems.map(nextElem => nextElem :: curPath).toList
        findTailrec(rest ++ nextCheckList, checked + curPath.head)
    }
    findTailrec(List(List(start)), Set(start))

    @tailrec
    def findPathTailrecV2(toCheck: List[List[T]], checked: Set[T]): List[T] = toCheck match {
      case Nil => List()
      case _ =>
        val firstRightPath = toCheck.find(_.head == target)
        if (firstRightPath.isDefined) firstRightPath.get.reverse
        else {
          val filteredPaths = toCheck.filterNot { case curHead :: restPath => checked.contains(curHead)}
          val nextLevel = filteredPaths.flatMap { case curHead :: restPath
          => graph.getOrElse(curHead, Set()).toList.map(nextElem => nextElem :: curHead :: restPath)
          }
          val nextChecked = toCheck.map(checkedPaths => checkedPaths.head).toSet ++ checked
          findPathTailrecV2(nextLevel, nextChecked)
        }
    }
    findTailrec(List(List(start)), Set.empty)
    findPathTailrecV2(List(List(start)), Set.empty)
  }

  //return a loop on the route that starts at 'node' (the loop does not have to contain the node)
  def findCycle[T](graph: Graph[T], node:T): List[T] = {
    @tailrec
    def findTailrec(toCheck: List[List[T]]): List[T] = toCheck match {
      case Nil => List()
      case curPath :: restPaths if curPath.isEmpty || !graph.contains(curPath.head) => findTailrec(restPaths)
      case curPath :: restPaths if curPath.drop(1).toSet.contains(curPath.head) =>
        val repetitionIndex = curPath.tail.indices.find(curPath.tail(_) == curPath.head).get
        curPath.slice(1, repetitionIndex + 2).reverse
      case curPath :: restPaths =>
        val nextCheckList = graph.getOrElse(curPath.head, Set()).toList.map(nextElem => nextElem :: curPath)
        findTailrec(restPaths ++ nextCheckList)
    }
    findTailrec(List(List(node)))
  }

  //return the original graph with all edges going both directions between Nodes
  def makeUndirectedV2[T](graph: Graph[T]): Graph[T] = {
    @tailrec
    val keys = graph.keySet
    keys.foldLeft(graph) { case (accMap, curKey) =>
      val curKeyFriends = graph.getOrElse(curKey, Set())
      curKeyFriends.foldLeft(accMap) { case (curKeyAcc, curFriend) =>
        curKeyAcc +
          (curFriend -> (curKeyAcc.getOrElse(curFriend, Set()) + curKey))
      }
    }
  }

  def makeUndirected[T](graph:Graph[T]): Graph[T] = {
    val nodes = graph.keySet.toList
    @tailrec
    def unDirectedTailrec(restNodes: List[T], acc: Graph[T]): Graph[T] = restNodes match {
      case Nil => acc
      case curNode :: rest => {
        val curNodesFriends = graph.getOrElse(curNode, Set())
        val newAcc = curNodesFriends.foldLeft(acc){ case (curAcc, curFriend) =>
          curAcc + (curFriend -> (curAcc.getOrElse(curFriend, Set()) + curNode))}
        unDirectedTailrec(rest, newAcc)
      }
    }
    unDirectedTailrec(nodes, graph)
  }

  // colour code all nodes in a way, that the nodes having the same colour can not be directly connected, nd there should be the least amount of colours

  def colourGraphV1[T](graph: Graph[T]): Map[T, Int] = {
    val undirectedGraph = makeUndirected(graph)
    val keySet = undirectedGraph.keySet.toList

    @tailrec
    def colourTailrec(rest: List[T], acc: List[(Int, Set[T])]): Map[T, Int] = rest match {
      case Nil => acc.flatMap { case (colour, elemsWThisCoulour) => elemsWThisCoulour.map(elem => (elem, colour)) }.toMap
      case curElem :: restElems => {
        val curFriends = undirectedGraph.getOrElse(curElem, Set())
        val curColour = acc.find { case (colour, elems) => !curFriends.exists(elems.contains) }
        val nextAcc = curColour match {
          case Some((colourIx, elems)) => acc.updated(colourIx, (colourIx, elems + curElem))
          case None => acc :+ (acc.size, Set(curElem))
        }
        colourTailrec(restElems, nextAcc)
      }
    }
    colourTailrec(keySet, List())
  }

   def colourGraphV2[T](graph: Graph[T]): Map[T, Int] = {
    val undirectedGraph = makeUndirected(graph)
    val keys = undirectedGraph.keySet.toList

    @tailrec
    def colourTailrec(toBeColoured: List[T], curColour: Int, coloured: Map[T, Int]): Map[T,Int] = toBeColoured match {
      case Nil => coloured
      case lastPerson :: Nil => coloured + (lastPerson -> curColour)
      case curPerson :: restPpl =>
        val (thisColour, notColouredPpl) = restPpl.foldLeft((Map(curPerson -> curColour), List.empty[T]))
        { case ((curAcc, curNextLevelPeople), nextPerson) =>
            val (nextAcc, nextLevelPeople)  = {
              val nextPFriends = undirectedGraph.getOrElse(nextPerson, Set())
              val canBeAddedToColourGroup = (curAcc.keySet & nextPFriends).isEmpty
              if (canBeAddedToColourGroup) (curAcc + (nextPerson -> curColour), curNextLevelPeople)
              else (curAcc, nextPerson :: curNextLevelPeople)
            }
          (nextAcc, nextLevelPeople)
        }
        colourTailrec(notColouredPpl, curColour + 1, coloured ++ thisColour)
      }
    colourTailrec(keys, 0, Map.empty)

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
  assert(makeUndirected(socialNetwork) == makeUndirectedV2(socialNetwork))
  println(colourGraphV1(socialNetwork))
  println(colourGraphV2(socialNetwork))


}
