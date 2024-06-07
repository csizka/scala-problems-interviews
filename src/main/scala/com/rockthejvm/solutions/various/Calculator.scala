package com.rockthejvm.solutions.various

import scala.annotation.tailrec

object Calculator extends App {

  def parseString(string: String): Vector[String] = {
    string.split(" ").toVector
  }
  def addOrSubstract(num1: Int, num2: Int, symbol: String): Int =  symbol match {
    case "+" => num1 + num2
    case _ => num1 - num2
  }
  def divideOrMultiply(num1: Int, num2: Int, symbol: String): Int = symbol match {
    case "*" => num1 * num2
    case _ => num1 / num2
  }

  def evaluate(str: String): Int = {
    val toCalculate = parseString(str)
    val allNums = toCalculate.flatMap(elem => elem.toIntOption match {
      case Some(num) => List(num)
      case None => None
    })
    val allSymbols = toCalculate.flatMap(elem =>
      if (Set("+", "-", "*", "/").contains(elem)) List(elem)
      else None)

    @tailrec
    def evaluateRecursive(curNums: Vector[Int], curSymbols: Vector[String]): Int = {
      if (curNums.length == 1) curNums.head
      else {
        val indexedSymbols = curSymbols.zipWithIndex
        val priorityOperation: Option[(String, Int)] = indexedSymbols.find{ case (elem, ix) => Set("*", "/").contains(elem)}
        // TODO: match on the Option instead
        priorityOperation match {
          case Some((symb, ix)) =>
            val (symbol, symbolIx) = (symb, ix)
            val partialResult = divideOrMultiply(curNums(symbolIx), curNums(symbolIx + 1), symbol)
            val (nextNumFstPart, nextNumLastPart) = curNums.splitAt(symbolIx + 1)
            val nextNums = nextNumFstPart.dropRight(1) ++ nextNumLastPart.updated(0, partialResult)
            val (nextSymbolFstPart, nextSymbolLastPart) = curSymbols.splitAt(symbolIx)
            val nextSymbols = nextSymbolFstPart ++ nextSymbolLastPart.drop(1)
            evaluateRecursive(nextNums, nextSymbols)
          case None =>
            val partialResult = addOrSubstract(curNums.head, curNums(1), curSymbols.head)
            evaluateRecursive(partialResult +: curNums.drop(2), curSymbols.drop(1))
        }
      }
    }
    evaluateRecursive(allNums, allSymbols)
  }

  val testString = " 1 + 3 * 45 - 2 * 10"
  val testString2 = " 100 + 2 * 3 - 2 * 10 + 6 / 3"
  val testString3 = " 10 / 5 * 4 + 2 * 10 - 1"

  assert(evaluate(testString) == 116)
  assert(evaluate(testString2) == 88)
  assert(evaluate(testString3) == 27)

}
