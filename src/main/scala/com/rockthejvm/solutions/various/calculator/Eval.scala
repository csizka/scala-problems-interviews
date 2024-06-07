package com.rockthejvm.solutions.various.calculator

import com.rockthejvm.solutions.various.calculator.ast._

import scala.annotation.tailrec
import scala.collection.mutable

object Eval {
  def buildExprList(expr: Expr): List[Expr] = {
    @tailrec
    def buildExprListTailRec(rest: List[Expr], res: List[Expr]): List[Expr] = rest match {
      case Nil          => res
      case head :: rest => head match {
        case Value(value) => buildExprListTailRec(rest, Value(value) :: res)
        case curExpr      => buildExprListTailRec(curExpr.children.reverse ++ rest, curExpr :: res)
      }

    }
    buildExprListTailRec(List(expr), List())
  }
  def evalExprList(exprList: List[ast.Expr]): Int = {
    // NOTE: mutates the input stack
    def evalOp(evalStack: mutable.Stack[Int], opFn: (Int, Int) => Int): Unit = {
      val rhs = evalStack.pop()
      val lhs = evalStack.pop()
      val res = opFn(lhs, rhs)
      evalStack.push(res)
    }

    val evalStack = mutable.Stack.empty[Int]
    exprList.foreach {
      case Value(n) => evalStack.push(n)
      case Add(_, _) => evalOp(evalStack, _ + _)
      case Substract(_, _) => evalOp(evalStack, _ - _)
      case Multiply(_, _) => evalOp(evalStack, _ * _)
      case Divide(_, _) => evalOp(evalStack, _ / _)
    }

    evalStack.pop()
  }
  def eval(expr: ast.Expr): Int = {
    val expList = buildExprList(expr)
    evalExprList(expList)
  }

  def evalNonTailRec(expr: ast.Expr): Int = {

    expr match {
      case Value(x) => x
      case Add(left, right) => evalNonTailRec(left) + evalNonTailRec(right)
      case Substract(left, right) => evalNonTailRec(left) - evalNonTailRec(right)
      case Multiply(left, right) => evalNonTailRec(left) * evalNonTailRec(right)
      case Divide(left, right) => evalNonTailRec(left) / evalNonTailRec(right)
    }
  }

}
