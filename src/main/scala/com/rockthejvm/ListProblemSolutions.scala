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
        case _ => toStringTailRec(tail, stringAcc + "," + head)
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
      case RNil => count
      case ::(head, tail) => lengthHelper(tail, count + 1)
    }
    lengthHelper(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseHelper(rest: RList[T], result: RList[T]): RList[T] = rest match {
      case RNil => result
      case ::(head, tail) => reverseHelper(tail, head :: result)
    }
    reverseHelper(this, RNil).reverse
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
  assert(testCons.tail.::(testCons2.head).toString == "[1,2,5,3,4]")
  assert(testCons.apply(1) == 2)
  assert(testCons.apply(0) == 1)
  assert(testCons.apply(4) == 4)
  try testCons2.apply(22) catch {
    case e: Exception => println(s"Exception found: $e")
  }
  try testCons2.apply(-3) catch {
    case e: Exception => println(s"Exception found: $e")
  }
  assert(testCons2.length == 5)
  assert(RNil.length == 0)
  assert(RNil.reverse == RNil)
  assert(testCons.reverse == 4 :: 3 :: 5 :: 2 :: 1 :: RNil)
}
