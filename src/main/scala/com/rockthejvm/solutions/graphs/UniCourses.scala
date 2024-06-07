package com.rockthejvm.solutions.graphs

import scala.annotation.tailrec

object UniCourses extends App {
  /*
    nCourses courses at uni, labeled 1 -> n
    prerequisites = List[(a,b)]
    (a,b) = b is required in order to take a

    Can you take all courses 1 .. n without breaking any prerequisite?
   */

  def dependencyMapper(nCourses: Int, prereq: List[(Int, Int)]): Map[Int, Set[Int]] = {
    prereq.foldLeft((1 to nCourses).map(course => (course, Set[Int]())).toMap)
    { case (acc, (dependant, course)) => acc + (course -> (acc.getOrElse(course, Set()) + dependant)) }
  }
  def canTakeAllCourses(nCourses: Int, prereq: List[(Int, Int)]): Boolean = {
    val coursePrereqs = dependencyMapper(nCourses, prereq)

    @tailrec
    def canTakeAllCourses(curMap: Map[Int, Set[Int]]): Boolean = {
      val nextLevel = curMap.map{ case (course, prereq) => course -> (prereq ++ prereq.flatMap(curPreReq => curMap.getOrElse(curPreReq, Set())))}
      val impossible = nextLevel.exists{ case (course, prereqs) => prereqs.contains(course)}
      if (impossible) false
      else if (nextLevel == curMap) true
      else canTakeAllCourses(nextLevel)
    }
    canTakeAllCourses(coursePrereqs)
  }

  //Give one order of the courses that is valid, if there is no valid way, return List()
  def coursesOrder(nCourses: Int, prereq: List[(Int, Int)]): List[Int] = {
    val dependencies = dependencyMapper(nCourses, prereq)

    @tailrec
    def ordererTailrec(coursesToOrder: List[Int], orderedCourses: List[Int]): List[Int] = coursesToOrder match {
      case Nil => orderedCourses
      case _ =>
        val (nextOrderedCourses, nextToOrder) = coursesToOrder.foldLeft((orderedCourses, List.empty[Int]))
        { case ((curOrderedCourses, toOrderInNextIteration), curCourseToConsider) =>
        val curCourseCanBeAdded = dependencies.getOrElse(curCourseToConsider, Set()).forall(curOrderedCourses.toSet.contains(_))
          if (curCourseCanBeAdded) (curCourseToConsider :: curOrderedCourses, toOrderInNextIteration)
          else (curOrderedCourses, curCourseToConsider :: toOrderInNextIteration)
        }
        if (nextToOrder.size < coursesToOrder.size)
        ordererTailrec(nextToOrder, nextOrderedCourses)
        else List()
    }
    ordererTailrec((1 to nCourses).toList, List())
  }

  def coursesOrderV2(nCourses: Int, prereq: List[(Int, Int)]): List[Int] = {
    val dependencies = dependencyMapper(nCourses, prereq)
    @tailrec
    def courseOrderTailrec(
      toDo: Set[Int],
      currentNodes: List[Int],
      visited: Set[Int],
      explored: Set[Int],
      acc: List[Int]): List[Int] = currentNodes match {
      case Nil if toDo.isEmpty => acc
      case Nil if explored.contains(toDo.head) || visited.contains(toDo.head) => {
        courseOrderTailrec(toDo.tail, List(), visited, explored, acc)
      }
      case Nil => courseOrderTailrec(toDo.tail, List(toDo.head), visited, explored, acc)
      case curNode :: restNodes if explored.contains(curNode) => courseOrderTailrec(toDo, restNodes, visited, explored, acc)
      case curNode :: restNodes if dependencies.getOrElse(curNode, Set()).isEmpty || visited.contains(curNode) => {
        courseOrderTailrec (toDo, restNodes, visited, explored + curNode, curNode :: acc)
      }
      case curNode :: restNodes => {
        val nextNodes = dependencies.getOrElse(curNode, Set())
        val cycle = currentNodes.exists(course => nextNodes.contains(course))
        if (!cycle) courseOrderTailrec(toDo, dependencies.getOrElse(curNode, Set()).toList ++ currentNodes, visited + curNode, explored, acc)
        else List()
      }
    }
    courseOrderTailrec(dependencies.keySet, List(), Set(), Set(), List())
  }

  val testList1 = List((1,2), (2,1))
  val testList2 = List((1,2), (3,1), (4,2), (4,3))
  val testList3 = List((1,2), (3,2), (1,3))
  val testList4 = List((1,2), (2,3), (3,1))

  assert(!canTakeAllCourses(2, testList1))
  assert(canTakeAllCourses(4, testList2))
  assert(canTakeAllCourses(3, testList3))
  assert(!canTakeAllCourses(3, testList4))
  assert(coursesOrder(2, testList1).isEmpty)
  assert(coursesOrder(3, testList4).isEmpty)
  assert(coursesOrder(4, testList2) == List(2, 1, 3, 4))
  assert(coursesOrder(3, testList3) == List(2, 3, 1))
  assert(coursesOrderV2(2, testList1).isEmpty)
  assert(coursesOrderV2(3, testList4).isEmpty)
  assert(coursesOrderV2(4, testList2) == List(2, 1, 3, 4))
  assert(coursesOrderV2(3, testList3) == List(2, 3, 1))
}
