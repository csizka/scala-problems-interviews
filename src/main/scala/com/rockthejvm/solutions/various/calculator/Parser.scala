package com.rockthejvm.solutions.various.calculator

import Lexer._

// TODO: implement map / flatMap for Either
case class Parser[T](parse: Vector[Token] => Either[String, (Vector[Token], T)]) {

  def run(rawExpr: String): Either[String, (Vector[Token], T)] = {
    parse(Lexer.tokenize(rawExpr)).flatMap { case (remainingTokens, res) =>
      if (remainingTokens.isEmpty)
        Right(remainingTokens -> res)
      else
        Left(s"Unfinished input: ${remainingTokens.mkString("[", ", ", "]")}")
    }
  }
  def eval(rawExpr: String): Either[String, T] = run(rawExpr).map(_._2)

  def combine(other: Parser[T]): Parser[T] = Parser { ogTokens =>
    val res1 = this.parse(ogTokens)
    val res2 = other.parse(ogTokens)

    (res1, res2) match {
      case (Right(res1), Right(res2)) => Right(res1)
      case (Right(res1), Left(_)) => Right(res1)
      case (Left(_), Right(res2)) => Right(res2)
      case (Left(err1), Left(_)) => Left(err1)
    }
  }

  def map[U](f: T => U): Parser[U] = Parser { ogTokens =>
    parse(ogTokens) match {
      case Right((rest, res)) => Right(rest, f(res))
      case Left(error) => Left(error)
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
        Right(rest -> ())
      else
        Left(s"Expected token '${expected}', got '${actual}'")
    case Vector() => unexpectedEndOfInputErr
  }

  val integer: Parser[Int] = Parser {
    case actual +: rest =>
      actual.toIntOption match {
        case Some(res) => Right(rest, res)
        case None => Left(s"Expected an integer, got '${actual}'")
      }
    case Vector() => unexpectedEndOfInputErr
  }

}