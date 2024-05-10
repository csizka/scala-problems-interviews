package com.rockthejvm.solutions.trees
import scala.annotation.tailrec

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
}
