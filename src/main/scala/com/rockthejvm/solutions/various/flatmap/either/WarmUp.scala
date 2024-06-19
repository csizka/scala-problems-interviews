package com.rockthejvm.solutions.various.flatmap.either

object Definitions {
  sealed trait Either[+L, +R] {
    def map[R2](f: R => R2): Either[L, R2]

    def flatMap[L1 >: L, R2](f: R => Either[L1, R2]): Either[L1, R2]

    def filterOrElse[L1 >: L](pred: R => Boolean, default: => L1): Either[L1, R]
  }

  case class Left[+L, +R](value: L) extends Either[L, R] {
    override def map[R2](f: R => R2): Either[L, R2] = Left(value)

    override def flatMap[L1 >: L, R2](f: R => Either[L1, R2]): Either[L1, R2] = Left(value)

    override def filterOrElse[L1 >: L](pred: R => Boolean, default: => L1): Either[L, R] = Left(value)
  }

  case class Right[+L, +R](value: R) extends Either[L, R] {
    override def map[R2](f: R => R2): Either[L, R2] = Right(f(value))

    override def flatMap[L1 >: L, R2](f: R => Either[L1, R2]): Either[L1, R2] = f(value)

    override def filterOrElse[L1 >: L](pred: R => Boolean, default: => L1): Either[L1, R] = {
      if (pred(value)) Right(value)
      else Left(default)
    }
  }

  object Either {
    def fail[L, R](l: L): Either[L, R] = Left(l)
    def pure[L, R](r: R): Either[L, R] = Right(r)

    // Define filter with flatMap + pure (don't forget to add tests!)
    def filterOrElseWithFlatMap[L, R](either: Either[L, R], pred: R => Boolean, default: => L): Either[L, R] =
      either.flatMap(value => if (pred(value)) pure(value) else fail(default))

    // BONUS: try to define map with flatMap + pure (don't forget to add tests!)
    def mapWithFlatMap[L, R1, R2](either: Either[L, R1], f: R1 => R2): Either[L, R2] =
      either.flatMap(value => pure(f(value)))
  }
}

object Tests extends App {
  import Definitions._

  // Right tests
  assert(Right(5).map(_ * 2) == Right(10))
  assert(Right(5).map(_.toString) == Right("5"))
  assert(Right(5).flatMap(x => if (x % 2 == 0) Right(x / 2) else Right((x - 1) / 2)) == Right(2))
  assert(Right(5).filterOrElse(_ % 2 == 0, "odd") == Left("odd"))
  assert(Right(6).filterOrElse(_ % 2 == 0, "odd") == Right(6))

  // pure tests
  assert(Either.pure(5).map(_ * 2) == Right(10))

  // None tests
  val emptyInt = Either.fail[String, Int]("no int")

  def f(x: Int): Boolean = x % 3 == 0
  def tooBigForMe(x: String): Boolean = x.length >= 5
  def g(x: Int): Int = x * 2


  //Left tests
  assert(emptyInt.map(_ * 2) == Left("no int"))
  assert(emptyInt.map(_.toString) == Left("no int"))
  assert(emptyInt.flatMap(x => if (x % 2 == 0) Right(x / 2) else Right((x - 1) / 2)) == Left("no int"))
  assert(emptyInt.filterOrElse(_ % 2 == 0, "nope") == Left("no int"))
  assert(emptyInt.filterOrElse(_ % 2 == 0, "nope") == Left("no int"))

  // Either Object tests
  assert(Either.filterOrElseWithFlatMap(Right(3), f, "nope") == Right(3))
  assert(Either.filterOrElseWithFlatMap(Right(4), f, "nope") == Left("nope"))
  assert(Either.filterOrElseWithFlatMap(emptyInt, f, "nope") == Left("no int"))
  assert(Either.filterOrElseWithFlatMap(Right("Ur isten very big"), tooBigForMe, "Pont jo" ) == Right("Ur isten very big"))
  assert(Either.filterOrElseWithFlatMap(Right("Smol"), tooBigForMe, "Pont jo" ) == Left("Pont jo"))
  assert(Either.mapWithFlatMap(Right(3), g) == Right(6))
  assert(Either.mapWithFlatMap(Left("Not an int"), g) == Left("Not an int"))
}