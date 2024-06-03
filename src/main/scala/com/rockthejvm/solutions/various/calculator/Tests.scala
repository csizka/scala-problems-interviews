package com.rockthejvm.solutions.various.calculator

import com.rockthejvm.solutions.various.calculator.Eval._
import com.rockthejvm.solutions.various.calculator.ast._

object Tests extends App {

  val ex1 = Substract(
    Multiply(
      Add(
        Value(3),
        Value(2)
      ),
      Value(5)
    ),
    Value(1)
  )

  val ex2 = Add(
    Multiply(
      Substract(
        Value(3),
        Value(1)
      ),
      Value(5)
    ),
    Value(11)
  )

  val ex3 = Divide(Value(6), Value(2))
  val ex4 = Divide(ex2, ex3)

  assert(evalNonTailRec(ex1) == 24)
  assert(evalNonTailRec(ex2) == 21)
  assert(evalNonTailRec(ex3) == 3)
  assert(evalNonTailRec(ex4) == 7)
  assert(eval(ex1) == 24)
  assert(eval(ex2) == 21)
  assert(eval(ex3) == 3)
  assert(evalExprList(buildExprList(ex4)) == 7)
}
