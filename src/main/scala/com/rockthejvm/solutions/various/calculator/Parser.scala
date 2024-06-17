package com.rockthejvm.solutions.various.calculator

import Lexer._

trait Functor[F[_]] {
  def map[T, U](f: T => U): F[U]
}

trait Monad[F[_]] {
  def flatMap[T, U](f: T => F[U]): F[U]
}

// TODO: NonEmptyVector
case class Parser[T](parse: Vector[Token] => Either[String, Vector[(Vector[Token], T)]]) {

  def run(rawExpr: String): Either[String, (Vector[Token], T)] = {
    parse(Lexer.tokenize(rawExpr)).flatMap { results =>
      val (validResults, invalidResults) = results.partition(_._1.isEmpty)
      (validResults, invalidResults) match {
        case (fstValidRes +: _, _) => Right(fstValidRes)
        case (Vector(), fstInvalidRes +: _) => Left(s"Unfinished input: ${fstInvalidRes._1.mkString("[", ", ", "]")}")
        case _ => throw new Exception("Unexpected error: there should be at least one valid or invalid result.")
      }
    }
  }
  def eval(rawExpr: String): Either[String, T] = run(rawExpr).map(_._2)

  def combine(other: Parser[T]): Parser[T] = Parser { ogTokens =>
    val res1 = this.parse(ogTokens)
    val res2 = other.parse(ogTokens)

    (res1, res2) match {
      case (Right(res1), Right(res2)) => Right(res1 ++ res2)
      case (Right(res1), Left(_)) => Right(res1)
      case (Left(_), Right(res2)) => Right(res2)
      case (Left(err1), Left(_)) => Left(err1)
    }
  }

  def andThen[U](other: Parser[U]): Parser[U] = Parser { ogTokens =>
    val intermedRes = this.parse(ogTokens)
    intermedRes.flatMap { results =>
      val otherResults = results.map{ case (restTokens, res) => other.parse(restTokens) }
      val (rightResults, leftResults) = otherResults.partition(_.isRight)
      if (!rightResults.isEmpty)  Right(rightResults.flatMap { case Right(curRes) => curRes})
      else leftResults.head
    }
  }

  def andThen2[U](other: Parser[U]): Parser[U] = this.flatMap( (x: T) => other)

//  def times(n: Int): Parser[List[T]] = ???

  def map[U](f: T => U): Parser[U] = Parser { ogTokens =>
    parse(ogTokens).map( _.map{ case (rest, res) => (rest, f(res))})
  }

  def flatMap[U](f: T => Parser[U]): Parser[U] = Parser { ogTokens =>
    val intermedRes = parse(ogTokens)
    intermedRes.flatMap { result =>
      val finalResults = result.map { case (restTokens, res) =>
        val otherParser = f(res)
        otherParser.parse(restTokens)
      }
      val (rightResults, leftResults) = finalResults.partition(_.isRight)
      if (!rightResults.isEmpty) Right(rightResults.flatMap { case Right(curRes) => curRes })
      else leftResults.head
    }
  }
}

object Parser {
  val unexpectedEndOfInputErr = Left("Unexpected end of input")

  // NOTE: function with variadic arguments (vararg)
  def alternatives[T](fstParser: Parser[T], otherParsers: Parser[T]*): Parser[T] = {
    otherParsers.foldLeft(fstParser){ _ combine _ }
  }

  def alternativesV2[T](fstParser: Parser[T], otherParsers: Parser[T]*): Parser[T] = Parser { ogTokens =>
    val resOption = (fstParser +: otherParsers).map(parser => parser.parse(ogTokens)).find(_.isRight)
    resOption.fold(fstParser.parse(ogTokens))(identity)
  }

  // NOTE: this is a partial function bascially a match on the input param
  def token(expected: String): Parser[Unit] = Parser {
    case actual +: rest =>
      if (actual == expected)
        Right(Vector(rest -> ()))
      else
        Left(s"Expected token '${expected}', got '${actual}'")
    case Vector() => unexpectedEndOfInputErr
  }

  val integer: Parser[Int] = Parser {
    case actual +: rest =>
      actual.toIntOption match {
        case Some(res) => Right(Vector((rest, res)))
        case None => Left(s"Expected an integer, got '${actual}'")
      }
    case Vector() => unexpectedEndOfInputErr
  }

  val value: Parser[ast.Value] = integer.map(n => ast.Value(n))

  // if we need to parse after a step, we need to call flatMap
  // same as opPlus just without the syntax sugar
  val opPlus_desugared: Parser[ast.Expr] =
    value.flatMap { lhs =>
      token("+").flatMap { _ =>
        value.map { rhs =>
          ast.Add(lhs, rhs)
        }
      }
    }

  val opPlus: Parser[ast.Expr] = for {
    lhs <- value
    _ <- token("+")
    rhs <- value
  } yield ast.Add(lhs, rhs)

  // TODO: define a parser that can parse any operator (define a helper that can parse an op based on an input param): alternatives + helper

}