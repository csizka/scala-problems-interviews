package com.rockthejvm

import com.rockthejvm.RList.concatenateReverseFirstList

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def headOption: Option[T]
  def tailOption: Option[RList[T]]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem,this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  def ++[S >: T](lst: RList[S]): RList[S]
  def remove(index: Int): RList[T]
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def flatMapV2[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]
  def countGroups: RList[(T, Int)]
  def runLengthEncodingConseq: RList[(T, Int)]
  def repeatElems(n: Int): RList[T]
  def shiftLeft(n: Int): RList[T]
  def getRandElems(n: Int): RList[T]
  def getRandElemsV2(n: Int, seed: Option[Long]): RList[T]
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
  def insertInOrder[S >: T](elem: S, ordering: Ordering[S]): RList[S] = {
    @tailrec
    def insertHelper(elem: S, inspected: RList[S], rest: RList[S]): RList[S] = rest match {
      case RNil => (elem :: inspected).reverse
      case ::(head, tail) =>
        if (ordering.gt(elem, head)) insertHelper(elem, head :: inspected, tail)
        else concatenateReverseFirstList(elem :: inspected, rest)
    }
    insertHelper(elem, RNil, this)
  }
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
  def mergeSortedLists[S >: T](ordering: Ordering[S], toMergeWith: RList[S]): RList[S]
  def quickSort[S >: T](ordering: Ordering[S]): RList[S]
  def quickSortV2[S >: T](ordering: Ordering[S]): RList[S]
  def zip[S](other: RList[S]): RList[(T, S)]
  def all(pred: T => Boolean): Boolean
  def isSortedVOf[S >: T](list: RList[S], ordering: Ordering[S]): Boolean
  def contains[S >: T](elem: S): Boolean
}

case object RNil extends RList[Nothing] {
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def headOption: Option[Nothing] = None
  override def tailOption: Option[RList[Nothing]] = None
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
  override def ++[S >: Nothing](lst: RList[S]): RList[S] = lst
  override def remove(index: Int): RList[Nothing] = RNil
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def flatMapV2[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
  override def countGroups: RList[(Nothing, Int)] = RNil
  override def runLengthEncodingConseq: RList[(Nothing, Int)] = RNil
  override def repeatElems(n: Int): RList[Nothing] = RNil
  override def shiftLeft(n: Int): RList[Nothing] = RNil
  override def getRandElems(n: Int): RList[Nothing] = RNil
  override def getRandElemsV2(n: Int, seed: Option[Long] = None): RList[Nothing] = RNil
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def mergeSortedLists[S >: Nothing](ordering: Ordering[S], toMergeWith: RList[S]): RList[S] = toMergeWith
  override def quickSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def quickSortV2[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def zip[S](other: RList[S]): RList[(Nothing, S)] = RNil
  override def all(pred: Nothing => Boolean): Boolean = true
  override def isSortedVOf[S >: Nothing](list: RList[S], ordering: Ordering[S]): Boolean = list.isEmpty

  override def contains[S >: Nothing](elem: S): Boolean = false
}
case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def headOption: Option[T] = Some(this.head)
  override def tailOption: Option[RList[T]] = Option(this.tail)
  override def toString: String = {
    @tailrec
    def toStringTailRec(remainingElems: RList[T], stringAcc: String): String = remainingElems match {
      case RNil => stringAcc
      case ::(head, tail) => stringAcc match {
        case "" => toStringTailRec(tail, stringAcc + head)
        case _  => toStringTailRec(tail, stringAcc + "," + head)
      }
    }
    "[" + toStringTailRec(this, "") + "]"
  }
  override def apply(index: Int): T = {
    @tailrec
    def applyHelper(remainingList: RList[T], index: Int): T = index match {
      case 0 => remainingList.head
      case _ =>
        if (index > 0) applyHelper(remainingList.tail, index - 1)
        else throw new NoSuchElementException
    }
    applyHelper(this, index)
  }
  override def length: Int = {
    @tailrec
    def lengthHelper(remainingList: RList[T], count: Int): Int = remainingList match {
      case RNil           => count
      case ::(head, tail) => lengthHelper(tail, count + 1)
    }
    lengthHelper(this, 0)
  }
  override def reverse: RList[T] = {
    @tailrec
    def reverseHelper(rest: RList[T], acc: RList[T]): RList[T] = rest match {
      case RNil           => acc
      case ::(head, tail) => reverseHelper(tail, head :: acc)
    }
    reverseHelper(this, RNil)
  }
  override def ++[S >: T](lst: RList[S]): RList[S] = {
    concatenateReverseFirstList(this.reverse, lst)
  }
  override def remove(index: Int): RList[T] = {
    @tailrec
    def removeHelper(remaining: RList[T], accList: RList[T], counter: Int): RList[T] =remaining match{
      case RNil           => accList.reverse
      case ::(head, tail) =>
        if (counter == index) RList.concatenateReverseFirstList(accList, remaining.tail)
        else removeHelper(tail, head :: accList, counter + 1)
    }
    if (index >= 0) removeHelper(this, RNil, 0)
    else this
  }
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapHelper(rest: RList[T], acc: RList[S]): RList[S] = rest match {
      case RNil           => acc.reverse
      case ::(head, tail) => mapHelper(tail, f(head) :: acc)
    }
    mapHelper(this, RNil)
  }
  override def flatMapV2[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapHelper(remaining: RList[T], acc: RList[S]): RList[S] = remaining match {
      case RNil           => acc.reverse
      case ::(head, tail) => flatMapHelper(tail, f(head).reverse ++ acc )
    }
    flatMapHelper(this, RNil)
  }
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def betterFlatMapHelper(remaining: RList[T], acc: RList[RList[S]]): RList[S] = remaining match {
      case RNil => RList.concatenateAllReverseFirst(acc, RNil)
      case ::(head, tail) => betterFlatMapHelper(tail, f(head).reverse :: acc)
    }
    betterFlatMapHelper(this, RNil)
  }
  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterHelper(rest: RList[T], acc: RList[T]): RList[T] = rest match {
      case RNil           => acc.reverse
      case ::(head, tail) =>
        if (f(head)) filterHelper(tail, head :: acc)
        else filterHelper(tail, acc)
    }
    filterHelper(this, RNil)
  }
  override def countGroups: RList[(T, Int)] = {
    @tailrec
    def sorter(elem: T, rest: RList[(T, Int)], checked: RList[(T, Int)]): RList[(T, Int)] = rest match {
      case RNil                 =>  (elem, 1) :: checked
      case ::((fst, snd), tail) =>
        if (fst == elem) (elem, snd + 1) :: checked ++ tail
        else sorter(elem, tail, (fst, snd) :: checked)
    }
    @tailrec
    def rleHelper(rest: RList[T], acc: RList[(T, Int)]): RList[(T, Int)] = rest match {
      case RNil => acc
      case ::(head, tail) => rleHelper(tail, sorter(head, acc, RNil))
    }
    rleHelper(this, RNil)
  }
  override def runLengthEncodingConseq: RList[(T, Int)] = {
    @tailrec
    def rleConseqHelper(remaining: RList[T], acc: RList[(T, Int)]): RList[(T, Int)] = remaining match {
      case RNil           => acc.reverse
      case ::(head, tail) => acc match {
        case RNil                        => rleConseqHelper(tail, (head, 1) :: RNil)
        case ::((fstElem, fstNum), rest) =>
          if (head == fstElem) rleConseqHelper(tail, (head, fstNum + 1) :: rest)
          else rleConseqHelper(tail, (head, 1) :: acc)
      }
    }
    rleConseqHelper(this, RNil)
  }
  override def repeatElems(n: Int): RList[T] = {
    @tailrec
    def repeatHelper(rest: RList[T], count: Int, acc: RList[T]): RList[T] = rest match {
      case RNil           => acc.reverse
      case ::(head, tail) =>
        if (count > 1 ) repeatHelper(rest, count - 1, head :: acc)
        else if (count == 1) repeatHelper(tail, n, head :: acc)
        else rest
    }
    repeatHelper(this, n, RNil)
  }
  override def shiftLeft(n: Int): RList[T] = {
    @tailrec
    def shiftHelper(rest: RList[T], shiftCount: Int, res: RList[T]): RList[T] = {
      if (shiftCount < n) rest match {
        case RNil => shiftHelper(this, shiftCount, RNil)
        case ::(head, tail) => shiftHelper(tail, shiftCount + 1, head :: res )
      }
      else if (shiftCount == n) rest ++ res.reverse
      else this
    }
    shiftHelper(this, 0, RNil)
  }
  override def getRandElems(n: Int): RList[T] = {
    val size = this.length
    val rand = new Random(System.currentTimeMillis())
    @tailrec
    def getHelper(count: Int, acc: RList[T]): RList[T] = {
      if (count < n) getHelper(count + 1, this(rand.nextInt(size)) :: acc)
      else acc
    }
    getHelper(0, RNil)
  }
  override def getRandElemsV2(k: Int, seed: Option[Long] = None): RList[T] = {
    val rand = new Random(seed.getOrElse(System.currentTimeMillis()))
    RList.from(1 to k).map(x => this(rand.nextInt(this.length)))
  }
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def orderingHelper(rest: RList[S], acc: RList[S]): RList[S] = rest match {
      case RNil           => acc
      case ::(head, tail) => orderingHelper(tail, acc.insertInOrder(head, ordering))
    }
    orderingHelper(this, RNil)
  }
  override def mergeSortedLists[S >: T](ordering: Ordering[S], toMergeWith: RList[S]): RList[S] = {
    @tailrec
    def mergeHelper(list1: RList[S], list2: RList[S], acc: RList[S]): RList[S] =  list1 match {
      case RNil               => concatenateReverseFirstList(acc, list2)
      case ::(head1, tail1)   => list2 match {
        case RNil               => concatenateReverseFirstList(acc, list1)
        case ::(head2, tail2)   =>
          if (ordering.gt(head1, head2)) mergeHelper(list1, tail2, head2 :: acc)
          else mergeHelper(tail1, list2, head1 :: acc)
    }
    }
    mergeHelper(this, toMergeWith, RNil)
  }
  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def msTailRec(ls: RList[RList[S]], acc: RList[RList[S]]): RList[S] = ls match {
      case RNil => acc match {
        case RNil => RNil
        case head :: RNil => head
        case ::(head, tail) => msTailRec(acc, RNil)
        }
      case ::(fst, RNil)      => acc match {
        case RNil               => fst
        case ::(head, tail)     => msTailRec(fst :: acc, RNil)
        }
      case fst :: snd :: rest => msTailRec(rest, fst.mergeSortedLists(ordering, snd) :: acc)
    }
    msTailRec(this.map( x => x :: RNil), RNil)
  }

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def sortInitiator(rest: RList[S], elem: S, count: Int, smaller: RList[S], same: RList[S], bigger: RList[S]): RList[S] = rest match {
      case RNil           => sortHelper(smaller :: same :: bigger :: RNil, 0, count, RNil)
      case ::(head, tail) =>
        if (ordering.gt(head, elem)) sortInitiator(tail, elem, count + 1, smaller, same, head :: bigger)
        else if (ordering.lt(head, elem)) sortInitiator(tail, elem, count + 1, head :: smaller, same, bigger)
        else sortInitiator(tail, elem, count + 1, smaller, head :: same, bigger)
    }
    @tailrec
    def divider(rest: RList[S], elem: S, smaller: RList[S], bigger: RList[S]): RList[RList[S]] = rest match {
      case RNil   => (bigger :: (elem :: RNil) :: smaller :: RNil).filter(!_.isEmpty)
      case ::(head, tail) =>
        if (ordering.gt(head, elem)) divider(tail, elem, smaller, head :: bigger)
        else divider(tail, elem, head :: smaller, bigger)
    }
    @tailrec
    def sortHelper(rest: RList[RList[S]], count: Int, size: Int, acc: RList[RList[S]]): RList[S] =
      rest match {
        case RNil =>
          if (count == size) acc.flatMap(x => x).reverse
          else sortHelper(acc.reverse, 0, size, RNil)
        case ::(head, tail) => head match {
          case RNil => sortHelper(tail, count, size, acc)
          case ::(fstHead, fstTail) => sortHelper(tail, count + 1, size, divider(fstTail, fstHead, RNil, RNil) ++ acc)
        }
      }

    sortInitiator(tail, head, 0, RNil, head :: RNil, RNil)
  }
  override def quickSortV2[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def divider(rest:RList[S], elem: S, smaller: RList[S], same: RList[S], bigger: RList[S]): RList[RList[S]] = rest match {
      case RNil => smaller match {
        case RNil => bigger match {
          case RNil => same :: RNil
          case _    => same :: bigger :: RNil
        }
        case _ => bigger match {
          case RNil => smaller :: same :: RNil
          case _ => smaller :: same :: bigger :: RNil
        }
      }
      case ::(head, tail) =>
        if (ordering.gt(head, elem)) divider(tail, elem, smaller, same, head :: bigger)
        else if (ordering.lt(head, elem)) divider(tail, elem, head :: smaller, same, bigger)
        else divider(tail, elem, smaller, head :: same, bigger)
    }
    @tailrec
    def sortHelper(rest: RList[RList[S]], sorted: RList[S]): RList[S] = rest match {
      case RNil           => sorted.reverse
      case ::(head, tail) => head match {
        case RNil                 => sortHelper(tail, sorted)
        case ::(fstHead, RNil)    => sortHelper(tail, fstHead :: sorted)
        case ::(fstHead, fstTail) => sortHelper(divider(fstTail, fstHead, RNil, fstHead :: RNil, RNil) ++ tail, sorted)
      }
    }
    sortHelper(divider(tail, head, RNil, head :: RNil, RNil), RNil)
  }

  override def zip[S](other: RList[S]): RList[(T, S)] = {
    @tailrec
    def zipHelper(lhs: RList[T], rhs: RList[S], acc: RList[(T,S)]): RList[(T,S)] = (lhs, rhs) match {
      case (::(lhsHead, lhsTail), ::(rhsHead, rhsTail) ) => zipHelper(lhsTail, rhsTail, (lhsHead, rhsHead) :: acc)
      case _ => acc.reverse
    }
    zipHelper(this, other, RNil)
  }

  override def all(pred: T => Boolean): Boolean = {
    @tailrec
    def allHelper(list: RList[T]): Boolean = list match{
      case ::(head, tail) =>
        if (pred(head)) allHelper(tail)
        else false
      case RNil => true
    }
    allHelper(this)
  }
  override def isSortedVOf[S >: T](list: RList[S], ordering: Ordering[S]): Boolean = {
    val sameSize = this.length == list.length
    val zippedSortedList = this.zip(tail)
    val isInOrder = zippedSortedList.all { case (x, y) => ordering.lteq(x, y) }
    val thisGrouped = this.countGroups
    val isThisSubsetOfList = list.countGroups.all(thisGrouped.contains(_))
    sameSize && isInOrder && isThisSubsetOfList
  }

  override def contains[S >: T](elem: S): Boolean = {
    @tailrec
    def containsHelper(rest: RList[S]): Boolean = rest match {
      case RNil => false
      case ::(head, tail) =>
        if (head == elem) true
        else containsHelper(tail)
    }
    containsHelper(this)
  }
}
 object RList {
   def from[T](iterable: Iterable[T]): RList[T] = {
     @tailrec
     def convertToRList(remaining: Iterable[T], acc: RList[T]): RList[T] = {
       if (remaining.isEmpty) acc
       else convertToRList(remaining.tail, remaining.head :: acc)
     }
    convertToRList(iterable, RNil)
   }
   @tailrec
   def concatenateAllReverseFirst[S](elems: RList[RList[S]], acc: RList[S]): RList[S] = elems match {
     case RNil => acc
     case ::(head, tail) => head match {
       case RNil => concatenateAllReverseFirst(tail, acc)
       case ::(fst, rest) => concatenateAllReverseFirst(rest :: tail, fst :: acc)
     }
   }
   @tailrec
   def concatenateReverseFirstList[S](remaining: RList[S], acc: RList[S]): RList[S] = remaining match {
     case RNil => acc
     case ::(head, tail) => concatenateReverseFirstList(tail, head :: acc)
   }
 }
object ListProblemSolutions extends App {

  val testCons = ::(1, ::(2, ::(5, ::(3, ::(4, RNil)))))
  val testCons2 = 1 :: 2 :: 5 :: 3 :: 4 :: RNil
  val testCons3 = 5 :: 3 :: 4 :: RNil
  val testCons4 = 1 :: 2 :: RNil
  val aLargeList = RList.from(1 to 50000)
  val testCons5 = testCons ++ testCons4 ++ testCons4 ++ testCons
  val testCons6 = testCons.flatMap{
    x =>
      if (x % 2 == 0) x :: x :: x :: RNil
      else x :: 2 * x :: RNil}
  val trueList = true :: RNil
  assert(testCons.tail.::(testCons2.head).toString == "[1,2,5,3,4]")
  assert(testCons.apply(1) == 2)
  assert(testCons.apply(0) == 1)
  assert(testCons.apply(4) == 4)
//  try testCons2.apply(22) catch {
//    case e: Exception => println(s"Exception found: $e")
//  }
//  try testCons2.apply(-3) catch {
//    case e: Exception => println(s"Exception found: $e")
//  }
  assert(testCons2.length == 5)
  assert(RNil.length == 0)
  assert(RNil.reverse == RNil)
  assert(testCons.reverse == 4 :: 3 :: 5 :: 2 :: 1 :: RNil)
  assert(testCons4 ++ testCons3 == testCons)
  assert(testCons.remove(0) == ::(2, ::(5, ::(3, ::(4, RNil)))))
  assert(testCons.remove(3) == ::(1, ::(2, ::(5, ::(4, RNil)))))
  assert(testCons2.filter(_ % 2 == 0) == 2 :: 4 :: RNil)
  assert(testCons.map(x => x * 2) == 2 :: 4 :: 10 :: 6 :: 8 :: RNil)
  assert(testCons.flatMap(x => x :: x * 2 :: RNil) == 1 :: 2 :: 2 :: 4 :: 5 :: 10 :: 3 :: 6 :: 4 :: 8 :: RNil)
  assert(testCons.flatMapV2(x => x :: x * 2 :: RNil) == 1 :: 2 :: 2 :: 4 :: 5 :: 10 :: 3 :: 6 :: 4 :: 8 :: RNil)
  val time = System.currentTimeMillis()
  aLargeList.flatMap(x => x :: 2 * x :: RNil)
  val flatMapV1Time = System.currentTimeMillis() - time
  val secondTime = System.currentTimeMillis()
  aLargeList.flatMapV2(x => x :: 2 * x :: RNil)
  val flatMapV2Time = System.currentTimeMillis() - secondTime
  assert(testCons5.countGroups == (4,2) :: (2,4) :: (1,4) :: (3,2) :: (5,2) :: RNil)
  assert(testCons6.runLengthEncodingConseq == (1,1) :: (2,4) :: (5,1) :: (10,1) :: (3,1) :: (6,1) :: (4,3) :: RNil)
  assert(testCons3.repeatElems(3) == 5 :: 5 :: 5 :: 3 :: 3 :: 3 :: 4 :: 4 :: 4 :: RNil)
  assert(testCons.shiftLeft(2) ==  5 :: 3 :: 4 :: 1 :: 2 :: RNil)
  assert(testCons.shiftLeft(2) ==  testCons.shiftLeft(7))
  assert(testCons3.shiftLeft(3) == testCons3)
  assert(testCons3.shiftLeft(3) == testCons3.shiftLeft(9))
//  println(testCons2.getRandElems(7))
//  println(testCons2.getRandElemsV2(7))
  val ordering = Ordering.fromLessThan[Int](_ < _)
  assert(testCons2.insertionSort(ordering) == 1 :: 2 :: 3 :: 4 :: 5 :: RNil)
  assert(testCons.insertionSort(ordering).isSortedVOf(testCons, ordering))
  assert(testCons.mergeSort(ordering).isSortedVOf(testCons,ordering))
  assert(testCons.quickSort(ordering).isSortedVOf(testCons, ordering))
  assert(testCons2.zip(testCons3) == (1,5) :: (2,3) :: (5,4) :: RNil)
  assert(trueList.repeatElems(100000).all(identity))
  assert(!(trueList.repeatElems(100000) ++ (false :: RNil)).all(identity))
  val bigList = aLargeList.getRandElemsV2(5000, Some(42))
  assert(bigList.quickSort(ordering).isSortedVOf(bigList,ordering))
//  val randomList = aLargeList.getRandElemsV2(10000, Some(100)) // SO
// val randomList = aLargeList.getRandElemsV2(7000, Some(83)) // RunsForever
  val randomList = aLargeList.getRandElemsV2(5000, Some(42))
  val insertTest0 = System.currentTimeMillis()
  val insertionSortedList = randomList.insertionSort(ordering)
  val insertTime = System.currentTimeMillis() - insertTest0
  println(insertTime)
  val mergeTest0 = System.currentTimeMillis()
  val mergeSortedList = randomList.mergeSort(ordering)
  val mergeTime = System.currentTimeMillis() - mergeTest0
  println(mergeTime)
  assert(mergeSortedList == insertionSortedList)
  val testConss = ::(3,RNil)
  assert(testConss.mergeSort(ordering) == ::(3, RNil))
  val quickTest0 = System.currentTimeMillis()
  val quickSortedList = randomList.quickSort(ordering)
  val quickTime = System.currentTimeMillis() - quickTest0
  println(quickTime)
//  val quickTest2 = System.currentTimeMillis()
//  val quickSortedList2 = randomList.quickSortV2(ordering)
//  val quickTime2 = System.currentTimeMillis() - quickTest2
//  println(quickTime2)
//  val randomList2 = aLargeList.getRandElemsV2(7000, Some(83))
//  val quickTest3 = System.currentTimeMillis()
//  val quickSortedList3 = randomList2.quickSortV2(ordering)
//  val quickTime3 = System.currentTimeMillis() - quickTest3
//  println(quickTime3)
}
