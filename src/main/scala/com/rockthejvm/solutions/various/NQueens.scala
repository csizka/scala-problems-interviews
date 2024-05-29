package com.rockthejvm.solutions.various

import scala.annotation.tailrec

object NQueens extends App {
  type Row = List[Int]
  type Table = List[Row]
  def createTable(n: Int): Table = {
    val row: Row = (0 until n).map( _ => 0).toList
    val table: Table = (0 until n).map( _ => row).toList
    table
  }
  def nQueens(queenCount: Int, table: List[List[Int]]): Set[List[List[Int]]] = {
    val rowCount = table.size
    val indices = table.indices.toList

    def correctTable(table: List[List[Int]], curQCount: Int): Boolean = {
      val areRowsCorrect = indices.forall( rowIx => table(rowIx).sum <= 1)
      val areColumnsCorrect = indices.forall(colIx => indices.map( rowIx => table(rowIx)(colIx)).sum <= 1)
      val areDiagonalsCorrect: Boolean = {
        val descendingIxs = (0 until rowCount - 1) ++ indices.drop(1).dropRight(1).map(Ix => Ix * rowCount)
        val ascendingIxs = indices.drop(1).map(Ix => rowCount * rowCount - 1 - Ix) ++ indices.drop(1).dropRight(1).map(Ix => Ix * rowCount)
        val simpleList = table.flatten.zipWithIndex
        val descendingDiagIsCorrect = descendingIxs.forall(curIx => simpleList.foldLeft(0) {
          case (acc, (curValue, curNum)) =>
            if (((curNum - (curNum / rowCount - curIx / rowCount)) % rowCount == curIx % rowCount) && ((curNum % rowCount) >= (curIx % rowCount)) && ((curNum / rowCount) >= (curIx / rowCount))) acc + curValue
            else acc
        } <= 1)
        val ascendingDiagisCorrect = ascendingIxs.forall(curIx => simpleList.foldLeft(0) {
          case (acc, (curValue, curNum)) =>
            if (((curNum - (curIx / rowCount - curNum / rowCount)) % rowCount == curIx % rowCount) && (curNum % rowCount >= curIx % rowCount) && ((curNum / rowCount) <= (curIx / rowCount))) acc + curValue
            else acc
        } <= 1)
        descendingDiagIsCorrect && ascendingDiagisCorrect
      }
      val isQCountCorrect = table.foldLeft(0)((acc, row) => acc + row.sum) == curQCount
      areRowsCorrect && areColumnsCorrect && isQCountCorrect && areDiagonalsCorrect
    }

    @tailrec
    def nQueensRecursive(curOptions: Set[Table], curCount: Int): Set[Table] = {
      if (curCount >= queenCount) curOptions
      else {
      val nextOptions = curOptions.flatMap( table => indices.flatMap( rowIx => indices.map( colIx => table.updated(rowIx, table(rowIx).updated(colIx, 1)))))
      val correctNextTables = nextOptions.filter( curTable => correctTable(curTable, curCount + 1))
      nQueensRecursive(correctNextTables, curCount + 1)
      }
    }
    nQueensRecursive(Set(table), 0)
  }

  // TODO: better return type
  // TODO: we usually call it a chess "board", rather than a chess "table"
  def nQueensV2(finalQueenCount: Int, tableRowCount: Int): Set[List[Int]] = {
    val indices = (0 until tableRowCount).toList
    // TODO: maybe use List[((Int, Int), Boolean)] to better communicate intent
    // TODO: maybe use a more descriptive name, like "emptyBoard"
    val xYcoordValueList = indices.flatMap(rowIx => indices.map(colIx => (rowIx, colIx, 0)))
    // TODO: you could use simple parens here
    val allCoords = { for {
      rowIx <- indices
      colIx <- indices
    } yield (rowIx, colIx)
    }.toSet

    // TODO: move this outside of the score of nQueensV2, will be easier to test it, and the code will be easier to read
    def diagBlockedCoordsV1(curTable: List[(Int, Int, Int)], qCoords: (Int, Int)): Set[(Int, Int)] = {
      val (queenRow, queenCol) = qCoords
      // TODO: Set.empty[(Int, Int)]
      // TODO: this can be implemented with a filter
      allCoords.foldLeft(Set.empty: Set[(Int, Int)]) { case (acc, (curRow, curCol)) =>
        // TODO: please extract the predicates into vals, and give them names like "isOnSameDescendingDiagonalAsQueen"
        if ((curCol == (queenCol + (queenRow - curRow))) || ((curRow - queenRow) == (curCol - queenCol))) acc + ((curRow, curCol))
        else acc
      }
    }


    // TODO: move this outside too
    // TODO: we talked about filtering the coords based on predicates, but here you are enumarating all "blocked" coords
    //   and subtracting them from the entire set. instead, you could just filter the entire set based on predicates.
    //   something like this: allCoord.filterNot(coord => isOnSameRow(coord, queenCoords) || isOnSameCol(coord, queenCoords) || isOnSameDiagonal(coord, queenCoords))
    //   defining the helpers for the predicates are left as an exercise for the reader
    def nextQCoords(curTable: List[(Int, Int, Int)]): Set[(Int, Int)] = {
      // TODO: is this a filter?
      val queens = curTable.foldLeft (Set.empty: Set[(Int, Int)]) {case (acc, (row, col, value)) =>
        if (value == 1) acc + ((row, col))
        else acc}
      // TODO: how about rowIx and colIx renamed to queenX and queenY
      val blockedCoords = queens.flatMap{ case (rowIx, colIx) =>
        // TODO: "blockedRows" and "blockedCols" could be a better name
        val rowBlocked = indices.flatMap( curColIx => Set((rowIx, curColIx))).toSet
        val colBlocked = indices.flatMap( curRowIx => Set((curRowIx, colIx))).toSet
        val diagBlocked = diagBlockedCoordsV1(curTable, (rowIx, colIx))
        rowBlocked ++ colBlocked ++ diagBlocked}
      allCoords -- blockedCoords
    }

    // TODO: might be owrth renaming qCount to curQueenCount, and the outer finalQueenCount to desiredQueenCOunt or numQueensToPlace
    // TODO: might be better to use a Map[(Int, Int), Boolean]. or even a two Set[(Int, Int)]s representing the set of currently available coordinates
    //  and the set of currently placed queens
    @tailrec
    def nQueensBreadthFirst(curLevel: Set[List[(Int, Int, Int)]], qCount: Int): Set[List[Int]] = {
      // TODO: the function inside the flatMap always returns a singleton List => the flatMap can be replaced with a map
      // TODO: when the variables are not used in a pattern match (case), we usually prefix it with an underscore: case (_row, _col, value) => ...
      if (qCount >= finalQueenCount) curLevel.flatMap( table => List(table.map{ case (row, col, value) => value}))
      else {
        // TODO: when the expression of a lambda consists of multiple lines (or begins with case), we use braces {} instead of parens ()
        val nextLevel = curLevel.flatMap( curTable =>
          // TODO: might be worth introduced a new variable here like "potentialCoordsForNextQueen", then mapping on it
          nextQCoords(curTable).map{ case (rowIx, colIx) =>
            // TODO: unnecessary braces around first argument of updated
            curTable.updated({rowIx * tableRowCount + colIx}, (rowIx, colIx, 1))})
        nQueensBreadthFirst(nextLevel, qCount + 1)
      }
    }

    @tailrec
    def notBlockedCoordsRecursive(restQueens: List[(Int, Int)], potentialCoords: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (restQueens.isEmpty) potentialCoords
      else {
        val (queenRow, queenCol) = restQueens.head
        val newPotCoords = potentialCoords.filterNot { case (rowIx, colIx) =>
          val sameRow = rowIx == queenRow
          val sameCol = colIx == queenCol
          val sameDiagonal = (colIx == (queenCol + (queenRow - rowIx))) || ((rowIx - queenRow) == (colIx - queenCol))
          sameRow || sameCol || sameDiagonal
        }
        notBlockedCoordsRecursive(restQueens.tail, newPotCoords)
      }
    }

    // TODO: indentation
nQueensBreadthFirst(Set(xYcoordValueList), 0)
  }

  val testTable1 = createTable(1)
  val testTable2 = createTable(2)
  val testTable3 = createTable(3)
  val testTable4 = createTable(4)
  val testTable5 = createTable(5)
  val testTable6 = createTable(6)
  val testTable7 = createTable(7)
  val testTable8 = createTable(8)
  val twoQueens = testTable8.updated(1, testTable8(1).updated(1,1)).updated(3, testTable8(3).updated(6,1))
  val notCorrectQueens = twoQueens.updated(1, twoQueens(1).updated(6,1))

//  println(nQueens(2, twoQueens))
//  println(nQueens(3, notCorrectQueens))
//  println(nQueens(1, testTable1))
//  println(nQueens(2, testTable1))
//  println(nQueens(1, testTable2))
//  println(nQueens(2, testTable3))
  println(nQueensV2(4, 8))

}
