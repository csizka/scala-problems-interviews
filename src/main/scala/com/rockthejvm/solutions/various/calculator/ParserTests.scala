package com.rockthejvm.solutions.various.calculator

import com.rockthejvm.solutions.various.calculator.ast._

object ParserTests extends App {
  assert(Parser.token("asd").eval("asd") == Right(()))
  assert(Parser.token("asd").eval("asd qwe") == Left("Unfinished input: [qwe]"))
  assert(Parser.token("asd").eval("qwe") == Left("Expected token 'asd', got 'qwe'"))

  assert(Parser.integer.eval("123") == Right(123))
  // TODO: if we didnt want to parse integers beginning with zero, how would we do that?
  assert(Parser.integer.eval("0123") == Right(123))
  assert(Parser.integer.eval("0") == Right(0))

  {
    val p = Parser.token("asd").combine(Parser.token("qwe"))
    assert(p.eval("asd") == Right(()))
    assert(p.eval("qwe") == Right(()))
  }

  {
    val p = Parser.integer.map(_ * 2)
    assert(p.eval("11") == Right(22))
    assert(p.eval("0") == Right(0))
    assert(p.run("55") == Right(Vector(),110))
  }

  {
    val p = Parser.alternatives(
      Parser.token("qwe"),
      Parser.token("asd"),
      Parser.token("gff"),
      Parser.token("grw"),
    )
    assert(p.eval("asd") == Right(()))
    assert(p.eval("qwe") == Right(()))
    assert(p.eval("aaa") == Left("Expected token 'qwe', got 'aaa'"))
  }
  {
    val p = Parser.token("qwe") combine Parser.token("asd") combine Parser.token("gff")
  }


  {
    val p1 = Parser.token("case")
    val p2 = Parser.token("case") andThen Parser.token("class")
    val p = p1 combine p2
    println(p.eval("case class"))
    assert(p.eval("case class") == Right(()))
  }

  {
    val p1 = Parser.token("asd")
    val p2 = Parser.token("qwe")
    val p = Parser.integer.flatMap(n => if (n % 2 == 0) p1 else p2)
    assert(p.eval("3 asd") == Left("Expected token 'qwe', got 'asd'"))
    assert(p.eval("3 qwe") == Right(()))
  }

  {
    assert(Parser.opPlus_desugared.eval("13 + 2") == Right(Add(Value(13), Value(2))))
    assert(Parser.opPlus.eval("13 + 2") == Right(Add(Value(13), Value(2))))
    assert(Parser.opPlus.eval("13 - 2") == Left("Expected token '+', got '-'"))
  }
}