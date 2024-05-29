package com.rockthejvm.solutions.various

import scala.annotation.tailrec

object NQueens extends App {


  def notBlockedCoords(newQueenCoords: (Int, Int), potentialQueenCoords: Set[(Int, Int)]): Set[(Int, Int)] = {
      val (queenRow, queenCol) = newQueenCoords
      val newPotCoords = potentialQueenCoords.filterNot { case (rowIx, colIx) =>
        val isInSameRow = rowIx == queenRow
        val isInSameCol = colIx == queenCol
        val isInSameDescenDingDiagonal = rowIx - queenRow == colIx - queenCol
        val isInSameAscendingDiagonal = colIx == queenCol + (queenRow - rowIx)
        isInSameRow || isInSameCol || isInSameAscendingDiagonal || isInSameDescenDingDiagonal
      }
    newPotCoords
  }

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
  assert(isNQueensPossible(2,3))
  assert(!isNQueensPossible(3,3))
  assert(isNQueensPossible(0,4))
  assert(isNQueensPossible(5,6) != nQueensPossibleCoords(5,6).isEmpty)
  assert(isNQueensPossible(6,6) != nQueensPossibleCoords(6,6).isEmpty)
  assert(isNQueensPossible(1, 8))
  assert(isNQueensPossible(7, 8))
  assert(!isNQueensPossible(27, 8))
  assert(isNQueensPossible(9,8) != nQueensPossibleCoords(9,8).isEmpty)
  assert(isNQueensPossible(9,9) != nQueensPossibleCoords(9,9).isEmpty)
  assert(isNQueensPossible(9,10) != nQueensPossibleCoords(9,10).isEmpty)

}
