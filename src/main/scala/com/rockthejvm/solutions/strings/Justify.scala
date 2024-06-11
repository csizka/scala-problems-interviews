package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object Justify extends App {

  // StringBuilders https://www.baeldung.com/scala/stringbuilder
  def wordLength(text: String): List[(String, Int)] = {
    text.split(" ").toList.map(x => (x, x.length))
  }
  def justify(text: String, width: Int): String = {

    def distributeSpaces(words: List[String], charCount: Int): String = {
      val gapCount = words.size - 1
      val spacesToDistribute = width - charCount + gapCount
      val baseSpaceBatch = " ".repeat(spacesToDistribute / gapCount)
      val bonusSpaceBatch = baseSpaceBatch + " "
      val numBonusSpaces = spacesToDistribute % gapCount

      val curLine = new StringBuilder()
      words.foldLeft(0) { case (bonusSpacesAdded, curWord) =>
        if (curLine.isEmpty) {
          curLine.addAll(curWord)
          bonusSpacesAdded
        } else if (bonusSpacesAdded < numBonusSpaces) {
          curLine.addAll(bonusSpaceBatch).addAll(curWord)
          bonusSpacesAdded + 1
        } else {
          curLine.addAll(baseSpaceBatch).addAll(curWord)
          bonusSpacesAdded
        }
      }
      curLine.toString()
    }

    val wordLengths = wordLength(text)
    val (lastLineWords, lastWidth, prevLines) = wordLengths.foldLeft((List.empty[String], 0, List.empty[String])) {
      case ((curLineWords, curWidth, accLines), (curWord, curLength)) =>
        val nextLength = {
          if (curWidth > 0) curWidth + 1 + curLength
          else curLength
        }
        if (nextLength <= width)
          (curLineWords :+ curWord, nextLength, accLines)
        else
          (List(curWord), curLength, distributeSpaces(curLineWords, curWidth) :: accLines)
    }
    (distributeSpaces(lastLineWords, lastWidth) :: prevLines).reverse.mkString("\n")
  }
  val testTxt = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua." +
    "Vel fringilla est ullamcorper eget nulla facilisi. Ut enim blandit volutpat maecenas volutpat. " +
    "Vulputate sapien nec sagittis aliquam malesuada bibendum arcu vitae. Purus faucibus ornare suspendisse sed nisi lacus. " +
    "Integer vitae justo eget magna fermentum iaculis eu non. Nulla facilisi morbi tempus iaculis urna id volutpat lacus laoreet. " +
    "Vitae proin sagittis nisl rhoncus mattis rhoncus urna. Nibh mauris cursus mattis molestie a iaculis at erat pellentesque. " +
    "Arcu cursus euismod quis viverra nibh cras pulvinar mattis nunc. Iaculis nunc sed augue lacus viverra vitae congue eu consequat. " +
    "Suscipit tellus mauris a diam. Semper auctor neque vitae tempus. Elementum tempus egestas sed sed. Vitae turpis massa sed elementum tempus egestas. " +
    "Et netus et malesuada fames. Phasellus faucibus scelerisque eleifend donec pretium. Suspendisse sed nisi lacus sed viverra tellus in hac habitasse. " +
    "Diam phasellus vestibulum lorem sed risus ultricies tristique nulla aliquet."
  println(justify(testTxt, 50))


}
