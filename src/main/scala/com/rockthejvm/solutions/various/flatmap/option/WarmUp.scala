package com.rockthejvm.solutions.various.flatmap.option

object Definitions {
  sealed trait Option[+T] {
    def get: T

    def map[U](f: T => U): Option[U]

    def flatMap[U](f: T => Option[U]): Option[U]

    // if the pred is true, it returns the original Option, else None
    def filter(pred: T => Boolean): Option[T]
  }

  case class Some[T](value: T) extends Option[T] {
    override def get: T = value

    override def map[U](f: T => U): Option[U] = Some(f(value))

    override def flatMap[U](f: T => Option[U]): Option[U] = f(value)

    override def filter(pred: T => Boolean): Option[T] = {
      if (pred(value)) Some(value)
      else None
    }
  }

  case object None extends Option[Nothing] {
    override def get: Nothing = throw new Exception("Option was none")

    override def map[U](f: Nothing => U): Option[U] = None

    override def flatMap[U](f: Nothing => Option[U]): Option[U] = None

    override def filter(pred: Nothing => Boolean): Option[Nothing] = None
  }

  object Option {
    def empty[T]: Option[T] = None

    // puts the x into an Option ("lifts x into the context of Option")
    def pure[T](x: T): Option[T] = Some(x)

    // BONUS: try to define filter with flatMap + pure (don't forget to add tests!)
    def filterWithFlatMap[T](opt: Option[T], pred: T => Boolean): Option[T] = {
      opt.flatMap(x => if (pred(x)) pure(x) else empty)
    }

    // BONUS: try to define map with flatMap + pure (don't forget to add tests!)
    def mapWithFlatMap[T, U](opt: Option[T], f: T => U): Option[U] = {
      opt.flatMap(someValue => pure(f(someValue)))
    }
  }
}

object Tests extends App {
  import Definitions._

  // Some tests
  assert(Some(5).map(_ * 2) == Some(10))
  assert(Some(5).map(_.toString) == Some("5"))
  assert(Some(5).flatMap(x => if (x % 2 == 0) Some(x / 2) else Some((x - 1) / 2)) == Some(2))
  assert(Some(5).filter(_ % 2 == 0) == None)
  assert(Some(6).filter(_ % 2 == 0) == Some(6))

  // pure tests
  assert(Option.pure(5).map(_ * 2) == Some(10))

  // None tests
  val emptyInt = Option.empty[Int]

  def f(x: Int): Boolean = x % 3 == 0

  assert(emptyInt.map(_ * 2) == None)
  assert(emptyInt.map(_.toString) == None)
  assert(emptyInt.flatMap(x => if (x % 2 == 0) Some(x / 2) else Some((x - 1) / 2)) == None)
  assert(emptyInt.filter(_ % 2 == 0) == None)
  assert(emptyInt.filter(_ % 2 == 0) == None)
  assert(Option.filterWithFlatMap(Some(3), f) == Some(3))
  assert(Option.filterWithFlatMap(Some(4), f) == None)
  assert(Option.filterWithFlatMap(emptyInt, f) == None)
}