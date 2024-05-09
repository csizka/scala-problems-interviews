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
  val testTreeV2 = BEnd
  val degenerateTree = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))

  println(testTreeV2.collectLeaves)
  println(testTree.collectLeaves)
  println(testTree.leafCount)
}
