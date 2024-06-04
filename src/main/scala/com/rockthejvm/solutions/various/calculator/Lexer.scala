package com.rockthejvm.solutions.various.calculator

object Lexer {
  type Token = String
  def tokenize(rawExpr: String): Vector[Token] = {
    rawExpr.split("""\s+""").toVector
  }
}
