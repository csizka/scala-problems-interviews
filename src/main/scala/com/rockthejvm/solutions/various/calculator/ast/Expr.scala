package com.rockthejvm.solutions.various.calculator.ast

sealed abstract class Expr {
  def children: List[Expr]
}
case class Value(value: Int) extends Expr {
  override def children: List[Expr] = List.empty
}

case class Add(left: Expr, right: Expr) extends Expr {
  override def children: List[Expr] = List(left, right)
}

case class Substract(left: Expr, right: Expr) extends Expr {
  override def children: List[Expr] = List(left, right)
}

case class Multiply(left: Expr, right: Expr) extends Expr{
  override def children: List[Expr] = List(left, right)
}

case class Divide(left: Expr, right: Expr) extends Expr {
  override def children: List[Expr] = List(left, right)
}

object Expr {
  def parse(rawExpr: String): Expr = { ???
   }
}
