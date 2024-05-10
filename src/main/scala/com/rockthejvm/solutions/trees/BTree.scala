package com.rockthejvm.solutions.trees
import scala.annotation.tailrec
import scala.collection.immutable.Queue


sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int
  def size: Int
  def nthLevelNodes(n: Int): List[BTree[T]]
  def mirrorNodes: BTree[T]
  def isSameStructureAs[S >: T](otherTree: BTree[S]): Boolean
  def isSymmetrical: Boolean
  def toList: List[T]
}
case object BEnd extends BTree[Nothing] {
  override def isEmpty: Boolean = true
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List()
  override def leafCount: Int = 0
  override val size: Int = 0
  override def nthLevelNodes(n: Int): List[BTree[Nothing]] = List.empty
  override def mirrorNodes: BTree[Nothing] = BEnd
  override def isSameStructureAs[S >: Nothing](otherTree: BTree[S]): Boolean = otherTree == BEnd
  override def isSymmetrical: Boolean = true

  override def toList: List[Nothing] = List()
}

case class BNode[+T] (override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty
  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def collectHelper(toCheck: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = toCheck match {
      case Nil => acc
      case curTree :: rest =>
        if (!curTree.isEmpty && curTree.isLeaf) collectHelper(rest, curTree +: acc)
        else if (!curTree.isEmpty) collectHelper(curTree.left +: curTree.right +: rest, acc)
        else collectHelper(rest, acc)
    }
    collectHelper(List(this), List.empty)
  }
  override def leafCount: Int = this.collectLeaves.length
  override val size: Int = left.size + right.size + 1

  override def nthLevelNodes(n: Int): List[BTree[T]] = {
    @tailrec
    def nthLevelHelper(curlevel: Int, curNodes: List[BTree[T]]): List[BTree[T]] = {
      if (curNodes.isEmpty) List()
      else if (curlevel == n) curNodes
      else nthLevelHelper(curlevel + 1, curNodes.flatMap{
        case BNode(value, left, right) => List(left, right).filter(!_.isEmpty)})
    }
    @tailrec
    def nthLevelTailrec(curlevel: Int, curNodes: List[BTree[T]]): List[BTree[T]] = {
      if (curNodes.isEmpty) List()
      else if (curlevel == n) curNodes
      else {
        val nextNodes = for {
          node <- curNodes
          child <- List(node.left, node.right) if !child.isEmpty
        } yield child
        nthLevelTailrec(curlevel + 1, nextNodes)
      }
    }
    if (n >= 0) nthLevelTailrec(0, List(this))
    else List()
  }

  override def mirrorNodes: BTree[T] = {
    @tailrec
    def mirrorTailrec(toDo: List[BTree[T]], discovered: Set[BTree[T]], done: List[BTree[T]]): BTree[T] = {
      if (toDo.isEmpty) done.head
      else if (toDo.head.isEmpty || toDo.head.isLeaf) mirrorTailrec(toDo.tail, discovered, toDo.head +: done)
      else if (discovered.contains(toDo.head)) mirrorTailrec(toDo.tail, discovered, BNode(toDo.head.value, done.head, done.tail.head) +: done.tail.tail)
      else mirrorTailrec(toDo.head.left +: toDo.head.right +: toDo, discovered + toDo.head, done)
    }
    mirrorTailrec(List(this), Set.empty, List.empty)
  }

  override def isSameStructureAs[S >: T](otherTree: BTree[S]): Boolean = {
    @tailrec
    def sameChecker(lhs: List[BTree[T]], rhs: List[BTree[S]]): Boolean = lhs match {
      case fstLhsTree :: rest =>
        val fstRhsTree = rhs.head
        if (fstLhsTree.isEmpty && fstRhsTree.isEmpty) sameChecker(rest, rhs.tail)
        else if (fstLhsTree.isLeaf && fstRhsTree.isLeaf) sameChecker(rest, rhs.tail)
        else {
          val lhsLeft = fstLhsTree.left
          val lhsRight = fstLhsTree.right
          val rhsLeft = fstRhsTree.left
          val rhsRight = fstRhsTree.right
          if (lhsLeft.isEmpty && rhsLeft.isEmpty && !lhsRight.isEmpty && !rhsRight.isEmpty) sameChecker(lhsRight :: rest, rhsRight :: rhs.tail)
          else if (!lhsLeft.isEmpty && !rhsLeft.isEmpty && lhsRight.isEmpty && rhsRight.isEmpty) sameChecker(lhsLeft :: rest, rhsLeft :: rhs.tail)
          else if (!lhsLeft.isEmpty && !rhsLeft.isEmpty && !lhsRight.isEmpty && !rhsRight.isEmpty) sameChecker(lhsLeft :: lhsRight :: rest, rhsLeft :: rhsRight :: rhs.tail)
          else false
        }
      case Nil => true
    }
    if (this.size == otherTree.size) sameChecker(List(this), List(otherTree))
    else false
  }

  override def isSymmetrical: Boolean = this.isSameStructureAs(this.mirrorNodes)

  override def toList: List[T] = {
    @tailrec
    def toListHelperPerLevel(rest: List[BTree[T]], acc: List[T]): List[T] = rest match {
      case Nil => acc
      case curTrees =>
        val curValues = curTrees.map(_.value)
        val nextNodes = curTrees.flatMap { case BNode(value, left, right) => List(left, right) }.filter(!_.isEmpty)
        toListHelperPerLevel(nextNodes, acc ++ curValues)
    }

    @tailrec
    def toListHelperPerLevelV2(rest: List[BTree[T]], acc: List[T]): List[T] = {
      if (rest.isEmpty) acc
      else {
        val curNodes = for {
          node <- rest
          child <- List(node.left, node.right) if !child.isEmpty
        } yield child
        toListHelperPerLevelV2(curNodes, acc ++ curNodes.map(_.value))
      }
    }

    @tailrec
    def toListHelperPreOrder(rest: List[BTree[T]], acc: Queue[T]): List[T] = rest match {
      case BNode(value, left, right) :: restTrees => toListHelperPreOrder(left :: right :: restTrees, acc :+ value)
      case BEnd :: restTrees => toListHelperPreOrder(restTrees, acc)
      case Nil => acc.toList
    }

    @tailrec
    def toListHelperInOrder(toDo: List[BTree[T]], opened: Set[BTree[T]], acc: Queue[T]): List[T] = {
      if (toDo.isEmpty) acc.toList
      else {
        val curTree = toDo.head
        if (curTree.isEmpty) toListHelperInOrder(toDo.tail, opened, acc)
        else if (curTree.isLeaf || opened.contains(curTree)) toListHelperInOrder(toDo.tail, opened, acc :+ curTree.value)
        else toListHelperInOrder(curTree.left :: curTree :: curTree.right :: toDo.tail, opened + curTree, acc)
      }
    }

    @tailrec
    def toListHelperPostOrder(toDo: List[BTree[T]], opened: Set[BTree[T]], acc: Queue[T]): List[T] = {
      if (toDo.isEmpty) acc.toList
      else {
        val curTree = toDo.head
        if (curTree.isEmpty) toListHelperPostOrder(toDo.tail, opened, acc)
        else if (curTree.isLeaf || opened.contains(curTree)) toListHelperPostOrder(toDo.tail, opened, acc :+ curTree.value)
        else toListHelperPostOrder(curTree.left :: curTree.right :: toDo, opened + curTree, acc)
      }
    }
    println(toListHelperPerLevel(List(this), List.empty))
    println(toListHelperPerLevelV2(List(this), List(value)))
    println(toListHelperPreOrder(List(this), Queue.empty))
    println(toListHelperInOrder(List(this), Set.empty,  Queue.empty))
    println(toListHelperPostOrder(List(this), Set.empty, Queue.empty))

    toListHelperPerLevelV2(List(this), List.empty)
  }

}
object BinaryTreeProblems extends App {
  val testTree = BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4, BEnd,
        BNode(5, BEnd, BEnd))
      ),
    BNode(6,
        BNode(7, BEnd, BEnd),
        BNode(8, BEnd, BEnd)
        )
  )
  val testTreeSameShape = BNode(6,
    BNode(13,
      BNode(44, BEnd, BEnd),
      BNode(0, BEnd,
        BNode(56, BEnd, BEnd))
    ),
    BNode(3,
      BNode(1, BEnd, BEnd),
      BNode(2, BEnd, BEnd)
    )
  )
  val testTreeNonSameShape1 = BNode(6,
    BNode(13,
      BNode(44, BEnd, BEnd),
      BNode(0, BEnd,BEnd)
    ),
    BNode(3,
      BNode(1, BEnd, BEnd),
      BNode(2, BEnd, BEnd)
    )
  )
  val testTreeNonSameShape2 = BNode(6,
    BNode(3,
      BNode(4, BEnd, BEnd),
      BNode(0, BNode(6, BEnd, BEnd),
        BEnd)
    ),
    BNode(3,
      BNode(1, BEnd, BEnd),
      BNode(2, BEnd, BEnd)
    )
  )
  val testTreeV2 = BEnd
  val degenerateTree = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))

  println(testTreeV2.collectLeaves)
  println(testTree.collectLeaves)
  assert(testTree.leafCount == 4)
  println(testTree.nthLevelNodes(2).map(_.value))
  println(testTree.mirrorNodes)
  assert(testTree.isSameStructureAs(testTreeSameShape))
  assert(!testTree.isSameStructureAs(testTreeNonSameShape1))
  assert(!testTree.isSameStructureAs(testTreeNonSameShape2))
  testTree.toList
}
