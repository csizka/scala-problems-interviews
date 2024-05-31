package com.rockthejvm.solutions.various

import scala.annotation.tailrec

object Calculator extends App {

  def parseString(string: String): Array[String] = {
    string.split(" ")
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
    val tocalculate = parseString(str)
    val allNums = tocalculate.filter (elem => elem.toIntOption.isDefined).map (_.toInt).toList
    val allSymbols = tocalculate.filter(elem => Set("+", "-", "*", "/").contains(elem)).toList

    @tailrec
    def evaluateRecursive(curNums: List[Int], curSymbols: List[String]): Int = {
      if (curNums.length == 1) curNums.head
      else {
        val indexedSymbols = curSymbols.zipWithIndex
        val priorityOperation: Option[(String, Int)] = indexedSymbols.find{ case (elem, ix) => Set("*", "/").contains(elem)}
        if (priorityOperation.isDefined) {
          val (symbol, symbolIx) = priorityOperation.get
          val partialResult = divideOrMultiply(curNums(symbolIx), curNums(symbolIx + 1), symbol)
          val (nextNumFstPart, nextNumLastPart) = curNums.splitAt(symbolIx + 1)
          val nextNums = nextNumFstPart.dropRight(1) ++ nextNumLastPart.updated(0, partialResult)
          val (nextSymbolFstPart, nextSymbolLastPart) = curSymbols.splitAt(symbolIx)
          val nextSymbols = nextSymbolFstPart ++ nextSymbolLastPart.drop(1)
          evaluateRecursive(nextNums, nextSymbols)
        } else {
          val partialResult = addOrSubstract(curNums.head, curNums(1), curSymbols.head)
          evaluateRecursive(partialResult :: curNums.drop(2), curSymbols.drop(1))
        }
      }
    }
    evaluateRecursive(allNums, allSymbols)
  }

  val testString = " 1 + 3 * 45 - 2 * 10"
  val testString2 = " 100 + 2 * 3 - 2 * 10 + 6 / 3"
  val testString3 = " 10 / 5 * 4 + 2 * 10 - 1"

  println(evaluate(testString))
  println(evaluate(testString2))
  println(evaluate(testString3))

}
