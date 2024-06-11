package com.rockthejvm.solutions.strings

object RansomNote extends App {

  def isRandsomConstructable(note: String, poolText: String): Boolean = {
    def addChar(map: Map[Char, Int], char: Char): Map[Char, Int] =
      map + (char -> (map.getOrElse(char, 0) + 1))

    val noteChars = note.foldLeft(Map.empty[Char, Int])(addChar)
    val poolChars = poolText.foldLeft(Map.empty[Char, Int])(addChar)
    noteChars.forall {case (char, count) => poolChars.getOrElse(char, 0) >= count}
  }

  val testNote = "ls.iorem ro ptme,tipsum ll aun"
  val testText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Vel fringilla est ullamcorper eget nulla facilisi. Ut enim"
  val testNote2 = "Lorem ipsum dolor sit/Adiipllreom"

  assert(isRandsomConstructable(testNote, testText))
  assert(!isRandsomConstructable(testNote2, testText))

}
