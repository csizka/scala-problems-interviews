package com.rockthejvm.solutions.nums


object ParseInteger extends App {

  // TODO: youll see in the comments below, but in general i'd restructure this function a bit:
  /* - filter out all the invalid chars in the beginning
     - parse out the sign
     - parse each digit separately
     - calculate the final number with a foldLeft

    This will result in multiple traversals of the list, degrading the perf a little bit,
    but the code will become much cleaner.
    During these exercises, the main goal should be readability. If the computational complexity doesnt degrade
    with a refactor that improves readability, do the refactor. Meybe leave a comment about the ideas on how to improve perf.
  */

  //Note: if you substract 48 from a digit Char, you get the digit as an Int example: '0' - 48 = 0
  //Note '-' - 48 = -3
  def parseInt(originalInput: String): Int = {
    val allDigits = (0 to 9).toSet
    val (sign, inputDigits) = originalInput.foldLeft((1, List.empty[Int])){ case ((sign, parsedDigits), curChar) => curChar - 48 match {
        case char if allDigits.contains(char) => (sign, parsedDigits :+ char)
        case char if char == -3 => (-1, parsedDigits)
        case _ => (sign, parsedDigits)
      }
    }
    val partialRes = inputDigits.foldLeft(0){ case (curRes, curDigit) => curRes match {
      case canContinue if curRes >= 0 => curRes * 10 + curDigit
      case tooBigValue if curRes < 0 => -1
      }
    }
    val result = (partialRes, sign) match {
      case (int, sign) if int >= 0 => int * sign
      case (int, sign) if int < 0 && sign == 1 => Int.MaxValue
      case (int, sign) if int < 0 && sign == -1 => Int.MinValue
      case _ => throw new Exception(s"sign should be ether 1 or -1, and is: '$sign', while the abs(result): '$partialRes'")
    }
    result
  }

  def parseIntV2(originalInput: String): Int = {
    val allDigits = (0 to 9).toSet
    val (sign, inputDigits) = originalInput.foldLeft((1, 0)) { case ((sign, parsedDigits), curChar) =>
      (curChar - 48, parsedDigits) match {
        case (char, curRes) if allDigits.contains(char) && curRes >= 0 => (sign, parsedDigits * 10 + char)
        case (char, curRes) if allDigits.contains(char) && curRes < 0 => (sign, -1)
        case (char, curRes) if char == -3 => (-1, parsedDigits)
        case _ => (sign, parsedDigits)
      }
    }
    val result = (inputDigits, sign) match {
      case (int, sign) if int >= 0 => int * sign
      case (int, sign) if int < 0 && sign == 1 => Int.MaxValue
      case (int, sign) if int < 0 && sign == -1 => Int.MinValue
      case _ => throw new Exception(s"sign should be ether 1 or -1, and is: '$sign', while the abs(result): '$inputDigits'")
    }
    result
  }


assert(parseInt("  -  2471299678973") == Int.MinValue)
assert(parseInt("  -  0") == 0)
assert(parseInt(" +  973 912 ") == 973912)
assert(parseInt("  247129 9789999 3") == Int.MaxValue)
assert(parseInt("  -  56kdgi23932") == -5623932)
  assert(parseIntV2("  -  2471299678973") == Int.MinValue)
  assert(parseIntV2("  -  0") == 0)
  assert(parseIntV2(" +  973 912 ") == 973912)
  assert(parseIntV2("  247129 9789999 3") == Int.MaxValue)
  assert(parseIntV2("  -  56kdgi23932") == -5623932)
}
