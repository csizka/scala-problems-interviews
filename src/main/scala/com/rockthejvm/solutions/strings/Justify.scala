package com.rockthejvm.solutions.strings

import scala.annotation.tailrec

object Justify extends App {

  @tailrec
  def wordLengths(remaining: String, curTupple: (String, Int), acc: List[(String, Int)]): List[(String, Int)] = {
    if (remaining.isEmpty && curTupple != ("", 0)) acc :+ curTupple
    else if (remaining.isEmpty && curTupple == ("", 0)) acc
    else if (remaining.head != ' ') wordLengths(remaining.tail, (curTupple._1 + remaining.head, curTupple._2 + 1), acc)
    else if (remaining.head == ' ' && curTupple != ("", 0)) wordLengths(remaining.tail, ("", 0), acc :+ curTupple)
    else wordLengths(remaining.tail, curTupple, acc)
  }

  def wordLengthV2(text: String): List[(String, Int)] = {
    text.split(" ").toList.map(x => (x, x.length))
  }
  def justify(text: String, width: Int): String = {

    def distributeSpaces(words: List[String], charCount: Int): String = {
      val gapCount = words.size - 1
      val spacesToDistributeCount = width - charCount + gapCount
      val spaceUnitCount = spacesToDistributeCount / gapCount
      val bonusSpaceCount = spacesToDistributeCount % gapCount
      @tailrec
      def distributeHelper(rest: List[String], bonusSpaces: Int, acc: String): String = {
        if (rest.isEmpty) acc
        else if (acc == "") distributeHelper(rest.tail, bonusSpaces, rest.head)
        else if (bonusSpaces <= 0) {
          val toAdd = " ".repeat(spaceUnitCount) + rest.head
          distributeHelper(rest.tail, bonusSpaces, acc + toAdd)
        } else {
          val toAdd = " ".repeat(spaceUnitCount + 1) + rest.head
          distributeHelper(rest.tail, bonusSpaces - 1, acc + toAdd)
        }
      }
      distributeHelper(words, bonusSpaceCount, "")
    }

    @tailrec
    def lines(remaining: List[(String, Int)], curLine: List[String], curWidth: Int, acc: List[String]): List[String] = remaining match {
      case (nextWord, nextWordLength) :: rest =>
        val nextLen = {
          if (curWidth > 0) curWidth + nextWordLength + 1
          else nextWordLength
        }
        if (nextLen < width) lines(rest, curLine :+ nextWord, nextLen, acc)
        else if (nextLen == width) lines(rest, List.empty, 0, acc :+ {{curLine :+ nextWord}.mkString(" ")})
        else {
          lines(rest, List(nextWord), nextWordLength, acc :+ distributeSpaces(curLine, curWidth))
        }
      case Nil =>
        if (curWidth > 0) acc :+ curLine.mkString(" ")
        else acc
    }
    val wordLengthsList = wordLengthV2(text)
    lines(wordLengthsList, List.empty, 0, List.empty).mkString("\n")
  }
  val testTxt = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Vel fringilla est ullamcorper eget nulla facilisi. Ut enim blandit volutpat maecenas volutpat. Vulputate sapien nec sagittis aliquam malesuada bibendum arcu vitae. Purus faucibus ornare suspendisse sed nisi lacus. Integer vitae justo eget magna fermentum iaculis eu non. Nulla facilisi morbi tempus iaculis urna id volutpat lacus laoreet. Vitae proin sagittis nisl rhoncus mattis rhoncus urna. Nibh mauris cursus mattis molestie a iaculis at erat pellentesque. Arcu cursus euismod quis viverra nibh cras pulvinar mattis nunc. Iaculis nunc sed augue lacus viverra vitae congue eu consequat. Suscipit tellus mauris a diam. Semper auctor neque vitae tempus. Elementum tempus egestas sed sed. Vitae turpis massa sed elementum tempus egestas. Et netus et malesuada fames. Phasellus faucibus scelerisque eleifend donec pretium. Suspendisse sed nisi lacus sed viverra tellus in hac habitasse. Diam phasellus vestibulum lorem sed risus ultricies tristique nulla aliquet."
  println(justify(testTxt, 50))


}
