package com.rockthejvm.solutions.graphs

import scala.annotation.tailrec

object UniCourses extends App {

  def canTakeAllCourses(nCorses: Int, prereq: List[(Int, Int)]): Boolean = {
    def prereqMapper(list: List[(Int, Int)]): Map[Int, Set[Int]] = {
      list.foldLeft((1 to nCorses).flatMap(course => Map(course -> Set[Int]())).toMap){ case (acc, (course, prereq)) => acc + (course -> (acc.getOrElse(course, Set.empty) + prereq)) }
    }
    val map = prereqMapper(prereq)

    @tailrec
    def canTakeAllCourses(curMap: Map[Int, Set[Int]]): Boolean = {
      val nextLevel = curMap.map{ case (course, prereq) => course -> (prereq ++ prereq.flatMap(curPreReq => curMap(curPreReq)))}
      val impossible = nextLevel.exists(course => course._2.contains(course._1))
      if (impossible) false
      else if (nextLevel == curMap) true
      else canTakeAllCourses(nextLevel)
    }
    canTakeAllCourses(map)
  }

  def coursesOrder(nCorses: Int, prereq: List[(Int, Int)]): List[Int] = {
    def prereqMapper(list: List[(Int, Int)]): Map[Int, Set[Int]] = {
      list.foldLeft((1 to nCorses).flatMap(course => Map(course -> Set[Int]())).toMap){case (acc, (course, prereq)) => acc + (course -> (acc.getOrElse(course, Set.empty) + prereq))}
    }

    val dependencies = prereqMapper(prereq)
    val firstCourses = dependencies.flatMap{ case (course, prereq) => if (prereq.isEmpty) List(course) else List() }.toList
    val restCourses = (1 to nCorses).toSet -- firstCourses

    @tailrec
    def orderer(restcourses: Set[Int], acc: List[Int]): List[Int] = {
      if (restcourses.isEmpty) acc
      else {
        val nextLevel = restcourses.filter(course => dependencies(course).subsetOf(acc.toSet))
        if (!nextLevel.isEmpty) orderer(restcourses -- nextLevel, acc ++ nextLevel.toList)
        else {
          List()
        }
      }
    }
    orderer(restCourses, firstCourses)
  }

  def coursesOrderV2(nCourses: Int, prereq: List[(Int, Int)]): List[Int] = {
    def dependencyMapper(prereq: List[(Int, Int)]): Map[Int,Set[Int]] = {
      prereq.foldLeft((1 to nCourses).map( course => (course, Set[Int]())).toMap){ case (acc, (dependant, course)) => acc + (course -> (acc.getOrElse(course, Set()) + dependant))}
    }
    val dependencies = dependencyMapper(prereq)
    @tailrec
    def courseOrderTailrec(toDo: Set[Int], currentNodes: List[Int], visited: Set[Int], explored: Set[Int], acc: List[Int]): List[Int] = {
      if (toDo.isEmpty && currentNodes.isEmpty) acc
      else if (currentNodes.isEmpty && (explored.contains(toDo.head) || visited.contains(toDo.head))) courseOrderTailrec(toDo.tail, List(), visited, explored, acc)
      else if (currentNodes.isEmpty) courseOrderTailrec(toDo.tail, List(toDo.head), visited, explored, acc)
      else if (explored.contains(currentNodes.head)) courseOrderTailrec(toDo, currentNodes.tail, visited, explored, acc)
      else if (dependencies(currentNodes.head).isEmpty || visited.contains(currentNodes.head)) courseOrderTailrec(toDo, currentNodes.tail, visited, explored + currentNodes.head, currentNodes.head :: acc)
      else {
        val nextNodes = dependencies(currentNodes.head)
        val cycle = currentNodes.exists(course => nextNodes.contains(course))
        if (!cycle) courseOrderTailrec(toDo, dependencies(currentNodes.head).toList ++ currentNodes, visited + currentNodes.head, explored, acc)
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
