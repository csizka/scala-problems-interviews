package com.rockthejvm.solutions.trees
import scala.annotation.tailrec
import scala.collection.immutable.{List, Queue}


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
  def surveillance: Int
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
  override def surveillance: Int = 0
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

  override def surveillance: Int = {
    @tailrec
    def surveillanceTailrec(
      toDo: List[(List[BTree[T]],BTree[T])],
      done: Set[BTree[T]],
      becameLeaf: Set[BTree[T]],
      acc: Int): Int = {
      if (toDo.isEmpty) acc
      else {
        val curTree = toDo.head._2
        val route = toDo.head._1
        val newRoute = curTree :: route
        val leftIsEnd = curTree.left.isEmpty || done.contains(curTree.left)
        val leftIsLeaf = curTree.left.isLeaf || becameLeaf.contains(curTree.left)
        val rightIsLeaf = curTree.right.isLeaf || becameLeaf.contains(curTree.right)
        val rightIsEnd = curTree.right.isEmpty || done.contains(curTree.right)
        val curBecameLeaf = !done.contains(curTree) && {
          if (curTree.left.isEmpty && !right.isEmpty) done.contains(curTree.right)
          else if (!curTree.left.isEmpty && right.isEmpty) done.contains(curTree.left)
          else done.contains(curTree.left) && done.contains(curTree.right)
        }
        val shouldHaveCamera = (leftIsEnd && rightIsLeaf) || (leftIsLeaf && rightIsEnd) || (leftIsLeaf && rightIsLeaf)
        val curTreeIsLeaf = leftIsEnd && rightIsEnd && !done.contains(curTree)
        val treeShouldBeSkipped = leftIsEnd && rightIsEnd && done.contains(curTree)

        if (treeShouldBeSkipped && route.isEmpty) acc
        else if (treeShouldBeSkipped && !route.isEmpty) surveillanceTailrec(toDo.tail, done, becameLeaf, acc)
        else if ((curTreeIsLeaf || shouldHaveCamera) && route.isEmpty) acc + 1
        else if (curBecameLeaf && !route.isEmpty)
          surveillanceTailrec(toDo.tail, done, becameLeaf + curTree, acc)
        else if (curTreeIsLeaf && !route.isEmpty)
          surveillanceTailrec(toDo.tail, done, becameLeaf, acc)
        else if (shouldHaveCamera && !route.isEmpty)
          surveillanceTailrec(toDo.tail, (((done + route.head) + curTree) + curTree.left) + curTree.right, (((becameLeaf - route.head) - curTree.left) - curTree.right) - curTree, acc + 1)
        else if (rightIsEnd) surveillanceTailrec((newRoute, curTree.left) :: toDo, done, becameLeaf, acc)
        else if (leftIsEnd) surveillanceTailrec((newRoute, curTree.right) :: toDo, done, becameLeaf, acc)
        else if (leftIsLeaf) surveillanceTailrec((newRoute, curTree.right) :: (newRoute, curTree.left) :: toDo, done, becameLeaf, acc)
        else surveillanceTailrec((newRoute, curTree.left) :: (newRoute, curTree.right) :: toDo, done, becameLeaf, acc)
      }
    }

    @tailrec
    def surveillanceV2(trees: List[BTree[T]], explored: Set[BTree[T]], evaluation: List[(Int, String)]): Int = {
      if (trees.isEmpty)
        if (evaluation.head._2 == "Not covered") evaluation.head._1 + 1
        else evaluation.head._1
      else {
        val curTree = trees.head
        if (curTree.isEmpty) surveillanceV2(trees.tail, explored, (0, "Covered") :: evaluation)
        else if (!explored.contains(curTree)) surveillanceV2(curTree.left :: curTree.right :: trees, explored + curTree, evaluation)
        else {
          val (leftCamCount, leftEvaluation) = evaluation.tail.head
          val (rightCamCount, rightEvaluation) = evaluation.head
          val (curCount, curEvaluation) = {
            if (leftEvaluation == "Not covered" || rightEvaluation == "Not covered") (leftCamCount + rightCamCount + 1, "Camera")
            else if (leftEvaluation == "Camera" || rightEvaluation == "Camera") (leftCamCount + rightCamCount, "Covered")
            else (leftCamCount + rightCamCount, "Not covered")
          }
          surveillanceV2(trees.tail, explored, (curCount, curEvaluation) :: evaluation.tail.tail)
        }
      }
    }
    surveillanceTailrec(List((List.empty,this)), Set.empty, Set.empty, 0)
    surveillanceV2(List(this), Set(), List())
  }

}


object BTree {

  def pathSum(tree: BTree[Int], target: Int): Boolean = {
    @tailrec
    def pathHelper(curTrees: List[(Int, BTree[Int])]): Boolean = {
      if (curTrees.isEmpty) false
      else if (curTrees.filter(_._1 == 0).exists(_._2.isLeaf)) true
      else {
        val nextTrees = {
          curTrees.flatMap {
            case (curSum, curTree) =>
              for {
                child <- List(curTree.left, curTree.right) if !child.isEmpty && curSum - child.value >= 0
              } yield (curSum - child.value, child)
          }
        }
        pathHelper(nextTrees)
      }
    }

    @tailrec
    def pathHelperV2(curTrees: List[(Int, BTree[Int])]): Boolean = {
      if (curTrees.isEmpty) false
      else if (curTrees.exists { case (sum, tree) => sum == target && tree.isLeaf }) true
      else {
        val nextTrees = curTrees flatMap { case (curSum, curTree) =>
          if (!curTree.left.isEmpty && !curTree.right.isEmpty) List((curSum + curTree.left.value, curTree.left), (curSum + curTree.right.value, curTree.right))
          else if (!curTree.left.isEmpty) List((curSum + curTree.left.value, curTree.left))
          else if (!curTree.right.isEmpty) List((curSum + curTree.right.value, curTree.right))
          else List()
        }
        pathHelperV2(nextTrees)
      }
    }

    pathHelper(List((target - tree.value, tree)))
    pathHelperV2(List((tree.value, tree)))
  }

  def findSumPaths(tree: BTree[Int], target: Int): List[List[Int]] = {
    @tailrec
    def findSumHelper(curTrees: Queue[BTree[Int]], paths: Queue[Queue[Int]], completedPaths: List[List[Int]]): List[List[Int]] = {
      val pathSum =
        if (paths.isEmpty) 0
        else paths.head.sum
      if (curTrees.isEmpty) completedPaths
      else if (curTrees.head.isEmpty) findSumHelper(curTrees.tail, paths.tail, completedPaths)
      else if (curTrees.head.isLeaf && (curTrees.head.value + pathSum) == target)
        findSumHelper(curTrees.tail, paths.tail, (paths.head :+ curTrees.head.value).toList :: completedPaths)
      else if (curTrees.head.isLeaf) findSumHelper(curTrees.tail, paths.tail, completedPaths)
      else if (paths.isEmpty) {
        val children = Queue(curTrees.head.left, curTrees.head.right).filter(!_.isEmpty)
        val nextPaths = children.map(child => Queue(curTrees.head.value))
        findSumHelper(curTrees.tail ++ children, nextPaths, completedPaths)
      }
      else {
        val children = Queue(curTrees.head.left, curTrees.head.right).filter(!_.isEmpty)
        val nextPaths = children.map(child => paths.head :+ curTrees.head.value)
        findSumHelper(curTrees.tail ++ children, paths.tail ++ nextPaths, completedPaths)
      }
    }

    @tailrec
    def findSumHelperV2(curTrees: List[(Int, List[Int], BTree[Int])], completedPaths: List[List[Int]]): List[List[Int]] = {
      if (curTrees.isEmpty) completedPaths
      else {
        val targetRes = curTrees.filter { case (sum: Int, path: List[Int], tree: BTree[Int]) => tree.isLeaf && (sum + tree.value) == target }
          .map({ case (sum, path, tree) => path :+ tree.value })
        val nextPaths = if (!targetRes.isEmpty) completedPaths ++ targetRes else completedPaths
        val nextTrees = {
          curTrees.flatMap { case (curSum, curPath, curTree) =>
            if (curTree.isLeaf || curTree.isEmpty) Queue()
            else Queue((curSum + curTree.value, curPath :+ curTree.value, curTree.left), (curSum + curTree.value, curPath :+ curTree.value, curTree.right))
          }
        }
        findSumHelperV2(nextTrees, nextPaths)
      }
    }

    findSumHelper(Queue(tree), Queue(), List.empty)
    findSumHelperV2(List((0, List.empty, tree)), List.empty)
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
  val testTreeV3 = BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4, BEnd,
        BNode(-1, BEnd, BEnd))
    ),
    BNode(6,
      BNode(7, BEnd, BEnd),
      BNode(8, BEnd, BEnd)
    )
  )
  val theUltimateTestTree =
    BNode(1,
      BNode(2,
        BNode(4,
          BNode(8,
            BNode(14, BEnd, BEnd),
            BEnd),
          BNode(9, BEnd, BEnd)),
        BNode(5,
          BEnd,
          BNode(10, BEnd, BEnd))),
      BNode(3,
        BNode(6,
          BNode(11, BEnd, BEnd),
          BNode(12,
             BEnd,
             BNode(15,BEnd,BEnd))),
        BNode(7,
          BEnd,
          BNode(13,BEnd,BEnd))))
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
  assert(!BTree.pathSum(testTree, 7))
  assert(BTree.pathSum(testTree, 6))
  assert(BTree.pathSum(testTree, 12))
  assert(!BTree.pathSum(testTree, 13))
  assert(!BTree.pathSum(testTree, 0))
  assert(BTree.pathSum(testTree, 14))
  assert(BTree.pathSum(testTree, 15))
  assert(BTree.findSumPaths(testTree, 7) == List())
  assert(BTree.findSumPaths(testTree, 13) == List())
  assert(BTree.findSumPaths(testTree, 6) == List(List(1,2,3)))
  assert(BTree.findSumPaths(testTree, 15) == List(List(1,6,8)))
  assert(BTree.findSumPaths(testTree, 14) == List(List(1,6,7)))
  assert(BTree.findSumPaths(testTreeV3, 6) == List(List(1, 2, 3), List(1, 2, 4, -1)))
  assert(testTree.surveillance == 3)
  assert(testTreeNonSameShape1.surveillance == 2)
  assert(theUltimateTestTree.surveillance == 7)
  assert(theUltimateTestTree.mirrorNodes.surveillance == 7)

}
