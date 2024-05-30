package com.rockthejvm.solutions.various

import scala.annotation.tailrec

object NQueens extends App {

// Method results in Set of coords not blocked after adding a new queen
  def notBlockedCoords(newQueenCoords: (Int, Int), potentialQueenCoords: Set[(Int, Int)]): Set[(Int, Int)] = {
      val (queenRow, queenCol) = newQueenCoords
      val newPotCoords = potentialQueenCoords.filterNot { case (rowIx, colIx) =>
        val isInSameRow = rowIx == queenRow
        val isInSameCol = colIx == queenCol
        val isInSameDescenDingDiagonal = colIx - rowIx == queenCol - queenRow
        val isInSameAscendingDiagonal = colIx + rowIx == queenCol + queenRow
        isInSameRow || isInSameCol || isInSameAscendingDiagonal || isInSameDescenDingDiagonal
      }
    newPotCoords
  }

// Generates a Board that has an empty queenCoords Set and the coords of an ixCount * ixCount board
  def genEmptyBoard(ixCount: Int): (Set[(Int,Int)], Set[(Int, Int)]) = {
    val indices = (0 until ixCount).toList
    val allCoords = (for {
      rowIx <- indices
      colIx <- indices
    } yield (rowIx, colIx)
      ).toSet
    val emptyBoard = (Set.empty[(Int, Int)], allCoords)
    emptyBoard
  }

//  ._._._._.
//  |_|_|_|_|
//  |_|_|_|_|
//  |_|_|_|_|
//  |_|_|_|_|

  //  ._._._._.


  //  |_|_|_|_   |
  //  |_|_|_|_   |
  //  |_|_|_|_   |
  //  |_|_|_|_   |

  // draw empty square,
  // draw square with Q
  // draw first line
  // add line-end to each row

  /*
    // gathers the queens for each line, and prints the lines 1-by-1
    queenCoordSet      :: Set[(Int, Int)]
      .groupBy(_._1)   :: Map[Int, Set[(Int, Int)]] // could specialize to exactly 1 queen in every row
      .sortBy(_._1)    :: List[(Int, Set[(Int, Int)])] // should we handle empty rows?
      .map(_._2)       :: List[Set[(Int, Int)]]
      .map(queensCoordsInRow => queenCordsInRow.map(_._2).toList)  :: List[List[Int]]
      .map(drawRow) :: List[String]
      .mkString("|\n")
   */

  def emptyBoard(n: Int): String = {
    val topBrick = "._"
    val generalBrick = "|_"
    val rowEnd = "|\n"
    val top = topBrick.repeat(n) + ".\n"
    val row = generalBrick.repeat(n) + rowEnd
    val body = row.repeat(n)
    top + body
  }

  def prettyPrintQCount(qCount: Int, boardSize: Int): String = {
    val topBrick = "._"
    val generalBrick = "|_"
    val rowEnd = "|\n"
    val top: String = topBrick.repeat(boardSize) + ".\n"
    val emptyRow = generalBrick.repeat(boardSize) + rowEnd
    val allBoards = nQueensPossibleCoords(qCount, boardSize).map( curboard => curboard.toList.sortBy{_._1})
    @tailrec
    def prettyPrintRecursive(restBoards: Set[List[(Int,Int)]], restQueens: List[(Int,Int)], curRow: Int, rowsAcc: String, boardsAcc: String): String = {
      if (restQueens.isEmpty && curRow == boardSize && restBoards.isEmpty) boardsAcc + rowsAcc
      else if (restQueens.isEmpty && curRow == boardSize) {
        prettyPrintRecursive(restBoards.tail, restBoards.head, -1, "", boardsAcc + rowsAcc)
      } else if (curRow == -1) prettyPrintRecursive(restBoards, restQueens, 0, top, boardsAcc)
      // TODO: run debug, and check this condition. run a few iterations and see why the recursion doesn't terminate
      else if (restQueens.isEmpty || restQueens.head._1 > curRow) {
        prettyPrintRecursive(restBoards, restQueens, curRow + 1, rowsAcc + emptyRow, boardsAcc)
      } else {
        val queenColIx = restQueens.head._2
        val firstPart = generalBrick.repeat(queenColIx) + "|X"
        val sndPart = generalBrick.repeat(boardSize - queenColIx - 1) + rowEnd
        val nextRow = firstPart + sndPart
        prettyPrintRecursive(restBoards, restQueens.tail, curRow + 1, rowsAcc + nextRow, boardsAcc)
      }
    }
    if (qCount <= 0)
      emptyBoard(boardSize)
    else {
      prettyPrintRecursive(allBoards, List.empty[(Int,Int)], boardSize,  "", "")
    }
  }

  def prettyPrintV2(qCount: Int, boardSize: Int): String = {
    val topBrick = "._"
    val generalBrick = "|_"
    val rowEnd = "|\n"
    val top = topBrick.repeat(boardSize) + ".\n"
    val emptyRow = generalBrick.repeat(boardSize) + rowEnd
    val indices = (0 until boardSize).toList
    val allBoards = nQueensPossibleCoords(qCount, boardSize)

    def printBoard(qcoords: Set[(Int, Int)]): String = {
      val queensByRows = indices.map( i => qcoords.filter(_._1 == i).map(_._2))
      val printList = queensByRows.map { qSet =>
        if (qSet.isEmpty) emptyRow
        else qSet.foldLeft(emptyRow){case (acc, colIx) => acc.substring(0, colIx * 2 + 1) + "X" + acc.substring(colIx * 2 + 2)}
      }
      top + printList.mkString + "\n"
    }
    allBoards.map( board => printBoard(board)).mkString
  }

  def nQueensPossibleCoords(finalQueenCount: Int, ixCount: Int): Set[Set[(Int,Int)]] = {
    val indices = (0 until ixCount).toList
    val emptyBoard = genEmptyBoard(ixCount)

    @tailrec
    def nQueensBreadthFirstRecursive(curBoards: Set[(Set[(Int,Int)], Set[(Int, Int)])], curQueenCount: Int): Set[Set[(Int,Int)]] = {
      if (curQueenCount >= finalQueenCount) curBoards.map{ case (queenCoords, potQueenCoords) => queenCoords }
      else {
        val nextBoards = curBoards.flatMap { case (queenCoords, potQueenCoords) =>
          val newQueensCoords = potQueenCoords
          newQueensCoords.map { curNewQueenCords =>
            val nextPotQueenCords = notBlockedCoords(curNewQueenCords, potQueenCoords)
            (queenCoords + curNewQueenCords, nextPotQueenCords) }}
        nQueensBreadthFirstRecursive(nextBoards, curQueenCount + 1)
      }
    }
    nQueensBreadthFirstRecursive(Set(emptyBoard), 0)
  }

  def isNQueensPossible(finalQueenCount: Int, ixCount: Int): Boolean = {
    val indices = (0 until ixCount).toList
    val emptyBoard = genEmptyBoard(ixCount)

    @tailrec
    def nQueensDepthFirstRecursive(
      restBoards: List[(Set[(Int,Int)], Set[(Int, Int)])],
      curBoard: (Set[(Int,Int)], Set[(Int, Int)]),
      curQueenCount: Int,
    ): Boolean = {
      val (curQueensCoords, curPotQueencoords) = curBoard

      if (curQueenCount >= finalQueenCount) {
        true
      } else if (curPotQueencoords.isEmpty && restBoards.isEmpty)
        false
      else if (curPotQueencoords.isEmpty) {
        val newCurBoard = restBoards.head
        val (newQueensCoords, newPotQueenCoords) = newCurBoard
        nQueensDepthFirstRecursive(restBoards.tail, newCurBoard, newQueensCoords.size)
      } else {
        val newBoards = curPotQueencoords.map { curNewQueenCoords =>
          val newPotQueenCoords = notBlockedCoords(curNewQueenCoords, curPotQueencoords)
          val newQueensCoords = curQueensCoords + curNewQueenCoords
          (newQueensCoords, newPotQueenCoords)
        }.toList
        nQueensDepthFirstRecursive(newBoards ++ restBoards, (Set.empty[(Int, Int)], Set.empty[(Int, Int)]), 0)
      }
    }
    nQueensDepthFirstRecursive(List.empty[(Set[(Int,Int)], Set[(Int, Int)])], emptyBoard, 0)
  }




//  println(nQueensBreadthFirst(4,8))
//  println(emptyBoard(8))
//  assert(isNQueensPossible(2,3))
//  assert(!isNQueensPossible(3,3))
//  assert(isNQueensPossible(0,4))
//  assert(isNQueensPossible(5,6) != nQueensPossibleCoords(5,6).isEmpty)
//  assert(isNQueensPossible(6,6) != nQueensPossibleCoords(6,6).isEmpty)
//  assert(isNQueensPossible(1, 8))
//  assert(isNQueensPossible(7, 8))
//  assert(!isNQueensPossible(27, 8))
//  assert(isNQueensPossible(9,8) != nQueensPossibleCoords(9,8).isEmpty)
//  assert(isNQueensPossible(9,9) != nQueensPossibleCoords(9,9).isEmpty)
//  assert(isNQueensPossible(9,10) != nQueensPossibleCoords(9,10).isEmpty)
//  println(prettyPrintQCount(8, 8))
  println(prettyPrintV2(7, 8))

}
