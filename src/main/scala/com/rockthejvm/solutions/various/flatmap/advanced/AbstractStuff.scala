package com.rockthejvm.solutions.various.flatmap.advanced

object AbstractStuff {
  // if both are Some, return a tuple of the results, otherwise None
  // try using flatMap or for (you can use match for deconstructing the tuple though)

  def tupledOption[T, U](tuple: (Option[T], Option[U])): Option[(T, U)] = {
    val (lhs, rhs) = tuple
    lhs.flatMap(val1 => rhs.flatMap(val2 => Some(val1, val2)))
  }

  def tupledOptionFor[T, U](tuple: (Option[T], Option[U])): Option[(T, U)] = {
    val (lhs, rhs) = tuple
    for {
      val1 <- lhs
      val2 <- rhs
    } yield (val1, val2)
  }

  // NOTE: the error types need to be the same
  // if both are Right, return the tuple of the results, otherwise the first error
  // try using flatMap or for (you can use match for deconstructing the tuple though)

  def tupledEither[L, R1, R2](tuple: (Either[L, R1], Either[L, R2])): Either[L, (R1, R2)] = {
    val (lhs, rhs) = tuple
    lhs.flatMap(val1 => rhs.flatMap(val2 => Right(val1, val2)))
  }

  def tupledEitherFor[L, R1, R2](tuple: (Either[L, R1], Either[L, R2])): Either[L, (R1, R2)] = {
    val (lhs, rhs) = tuple
    for {
      val1 <- lhs
      val2 <- rhs
    } yield (val1, val2)
  }

  def tupledEitherFlatMap[L, R1, R2](tuple: (Either[L, R1], Either[L, R2])): Either[L, (R1, R2)] =  {
    val (lhs, rhs) = tuple
    lhs.flatMap(val1 => rhs.flatMap(val2 => Right(val1, val2)))
  }


  // QUESTION (for tupled stuff): if you used flatMap, do you notice any similarities?
  // similar to tupled, but for a vector of options. if any fails, the whole thing fails

  def sequenceOption[T](xs: Vector[Option[T]]): Option[Vector[T]] = {
    xs.foldLeft(Some(Vector()): Option[Vector[T]]) { case (curState, curElem) =>
      curState.flatMap( vector => curElem.map(value => vector :+ value))
    }
  }

  def sequenceOptionFor[T](xs: Vector[Option[T]]): Option[Vector[T]] = {
    xs.foldLeft(Some(Vector()): Option[Vector[T]]) { case (curState, curElem) =>
      for {
        vector <- curState
        elem <- curElem
      } yield vector :+ elem
    }
  }

  // same as tupledEither, but for a vector of eithers. if any fails, the whole thing fails
  // note that the final failure should be the very first one
 def sequenceEither[L, R](xs: Vector[Either[L, R]]): Either[L, Vector[R]] = {
    xs.foldLeft(Right(Vector()): Either[L, Vector[R]]){ case (curState, curElem) =>
      curState.flatMap( vector => curElem.map(value => vector :+ value))
    }
  }

  // QUESTION (for sequence stuff): do you notice any similarities?
  // similar to sequenceOption, but it gets a function as input
  def traverseOption[T, U](xs: Vector[T], f: T => Option[U]): Option[Vector[U]] = {
    xs.foldLeft(Some(Vector()): Option[Vector[U]]) { case (curState, curElem) =>
      curState.flatMap( vector => f(curElem).map(value => vector :+ value))
    }
  }

  def traverseOptionWMap[T, U](xs: Vector[T], f: T => Option[U]): Option[Vector[U]] = {
    sequenceOption(xs.map(f))
  }

  // similar to sequenceOption, but it gets a function as input

  def traverseEither[L, R1, R2](xs: Vector[R1], f: R1 => Either[L, R2]): Either[L, Vector[R2]] = {
    xs.foldLeft(Right(Vector()): Either[L, Vector[R2]] ) { case (curState, curElem ) =>
      curState.flatMap( vector => f(curElem).map(value => vector :+ value))
    }
  }

  def traverseEitherWMap[L, R1, R2](xs: Vector[R1], f: R1 => Either[L, R2]): Either[L, Vector[R2]] = {
    sequenceEither(xs.map(f))
  }

  // QUESTION (for traverse stuff): do you notice any similarities?

  // as you can see tupled, sequence and traverse are basically both for Option and Either
  // in fact, this the implementation is the same for anything that supports flatMap + map + pure
  // (where we wtore Option(...) or Right(...) we could have written pure)
  // and since map can be expressed with flatMap + pure, we only really need those two,
  // and we can automatically define tupled, sequence and traverse for the type
}

object AbstractStuffTests extends App {
  import AbstractStuff._
  import com.rockthejvm.solutions.various.flatmap.option.Exercises._

  assert(tupledOption(Some(5), Some("asd")) == Some((5, "asd")))
  assert(tupledOption(Some(5), None) == None)
  assert(tupledOption(None, Some(5)) == None)
  assert(tupledOption(None, None) == None)

  // NOTE: usually the computations in the tuple are not so simple, for example:
  assert(
    tupledOption(
      getFilms("John", 2000),
      getFilms("Terry", 2003),
    ) == Some((
      List(flatLands, monadsEverywhere),
      List(startrekWars)
    ))
  )
  assert(
    tupledOption(
      getFilms("John", 2000),
      getFilms("Terrz", 2003),
    ) == None
  )


  assert(tupledEither(Right(5), Right("asd")) == Right((5, "asd")))
  assert(tupledEither(Right(5), Right("asd")) == tupledEitherFor(Right(5), Right("asd")))
  assert(tupledEither(Right(5), Right("asd")) == tupledEitherFlatMap(Right(5), Right("asd")))
  assert(tupledEither(Left("err1"), Right("asd")) == Left("err1"))
  assert(tupledEither(Left("err1"), Right("asd")) == tupledEitherFor(Left("err1"), Right("asd")))
  assert(tupledEither(Left("err1"), Right("asd")) == tupledEitherFlatMap(Left("err1"), Right("asd")))
  assert(tupledEither(Right(5), Left("err2")) == Left("err2"))
  assert(tupledEither(Left("err1"), Left("err2")) == Left("err1"))


  assert(sequenceOption(Vector()) == Some(Vector()))
  assert(sequenceOption(Vector()) == sequenceOptionFor(Vector()))
  assert(sequenceOption(Vector(Some(1), Some(2), Some(3))) == Some(Vector(1,2,3)))
  assert(sequenceOption(Vector(Some(1), Some(2), Some(3))) == sequenceOptionFor(Vector(Some(1), Some(2), Some(3))))
  assert(sequenceOption(Vector(Some(1), Some(2), None)) == None)
  assert(sequenceOption(Vector(Some(1), Some(2), None)) == sequenceOptionFor(Vector(Some(1), Some(2), None)))
  assert(sequenceOption(Vector(Some(1), None, Some(3))) == None)
  assert(sequenceOption(Vector(Some(1), None, Some(3))) == sequenceOptionFor(Vector(Some(1), None, Some(3))))
  assert(sequenceOption(Vector(None, Some(2), Some(3))) == None)
  assert(sequenceOption(Vector(None, None, None)) == None)

  // NOTE: again, the computations can be arbitrarily complex
  assert(
    sequenceOption(
      Vector(
        getFilms("John", 2000),
        getFilms("John", 1996),
        getFilms("John", 1993),
      )
    ) == Some(
      Vector(
        List(flatLands, monadsEverywhere),
        List(theBirth2),
        List(theBirth),
      )
    )
  )
//   NOTE (for the above): there's a bit of repetition here.
//   we are basically calling the same function with different inputs
//   can we maybe use a function instead ... ?
//   SPOILER: yes, see traverseOption


  assert(sequenceEither(Vector()) == Right(Vector()))
  assert(sequenceEither(Vector(Right(1), Right(2), Right(3))) == Right(Vector(1, 2, 3)))
  assert(sequenceEither(Vector(Right(1), Right(2), Left(false))) == Left(false))
  assert(sequenceEither(Vector(Right(1), Left(false), Right(3))) == Left(false))
  assert(sequenceEither(Vector(Left(false), Right(2), Right(3))) == Left(false))
  assert(sequenceEither(Vector(Left(true), Left(false), Left(false))) == Left(true))


  assert(
    traverseOption(
      Vector(
        2000,
        1996,
        1993
      ),
      getFilms("John", _)
    ) == Some(
      Vector(
        List(flatLands, monadsEverywhere),
        List(theBirth2),
        List(theBirth),
      )
    )
  )
  assert(
    traverseOption(
      Vector(
        2000,
        42,
        1993
      ),
      getFilms("John", _)
    ) == None
  )


  def divEither(x: Int) = if (x % 2 == 0) Right(x / 2) else Left(s"odd: ${x}")
  assert(traverseEither(Vector(2,4,6), divEither) == Right(Vector(1,2,3)))
  assert(traverseEither(Vector(1,4,6), divEither) == Left("odd: 1"))
  assert(traverseEither(Vector(1,4,3), divEither) == Left("odd: 1"))
  assert(traverseEither(Vector(2,4,3), divEither) == Left("odd: 3"))

}
