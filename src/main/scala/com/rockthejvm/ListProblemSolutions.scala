package com.rockthejvm

import scala.annotation.tailrec

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
  def runLengthEncodingNonConseq: RList[(T, Int)]
  def runLengthEncodingConseq: RList[(T, Int)]
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
  override def runLengthEncodingNonConseq: RList[(Nothing, Int)] = RNil
  override def runLengthEncodingConseq: RList[(Nothing, Int)] = RNil
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
  def concatenateReverseFirstList[S >: T](remaining: RList[S], acc: RList[S]): RList[S] = remaining match {
    case RNil           => acc
    case ::(head, tail) => concatenateReverseFirstList(tail, head :: acc)
  }
  override def ++[S >: T](lst: RList[S]): RList[S] = {
    concatenateReverseFirstList(this.reverse, lst)
  }

  override def remove(index: Int): RList[T] = {
    @tailrec
    def removeHelper(remaining: RList[T], accList: RList[T], counter: Int): RList[T] =remaining match{
      case RNil           => accList.reverse
      case ::(head, tail) =>
        if (counter == index) concatenateReverseFirstList(accList, remaining.tail)
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
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapHelper(remaining: RList[T], acc: RList[S]): RList[S] = remaining match {
      case RNil           => acc
      case ::(head, tail) => flatMapHelper(tail, acc ++  f(head))
    }
    flatMapHelper(this, RNil)
  }
  def flatMapV2[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapHelper(remaining: RList[T], acc: RList[S]): RList[S] = remaining match {
      case RNil => acc.reverse
      case ::(head, tail) => flatMapHelper(tail, f(head).reverse ++ acc )
    }
    flatMapHelper(this, RNil)
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

  override def runLengthEncodingNonConseq: RList[(T, Int)] = {
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
 }
object ListProblemSolutions extends App {

  val testCons = ::(1, ::(2, ::(5, ::(3, ::(4, RNil)))))
  val testCons2 = 1 :: 2 :: 5 :: 3 :: 4 :: RNil
  val testCons3 = 5 :: 3 :: 4 :: RNil
  val testCons4 = 1 :: 2 :: RNil
  val aLargeList = RList.from(1 to 10000)
  val testCons5 = testCons ++ testCons4 ++ testCons4 ++ testCons
  val testCons6 = testCons.flatMap{
    x =>
      if (x % 2 == 0) x :: x :: x :: RNil
      else x :: 2 * x :: RNil}
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
  val time = System.currentTimeMillis()
  aLargeList.flatMap(x => x :: 2 * x :: RNil)
  val flatMapV1Time = System.currentTimeMillis()-time
  val secondTime = System.currentTimeMillis()
  aLargeList.flatMapV2(x => x :: 2 * x :: RNil)
  val flatMapV2Time = System.currentTimeMillis() - secondTime
  assert(flatMapV1Time > flatMapV2Time)
  assert(testCons5.runLengthEncodingNonConseq == (4,2) :: (2,4) :: (1,4) :: (3,2) :: (5,2) :: RNil)
  assert(testCons6.runLengthEncodingConseq == (1,1) :: (2,4) :: (5,1) :: (10,1) :: (3,1) :: (6,1) :: (4,3) :: RNil)
}
