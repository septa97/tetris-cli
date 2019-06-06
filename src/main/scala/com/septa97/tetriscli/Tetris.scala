package com.septa97.tetriscli

import scala.io.StdIn

class Tetris {
  import Tetris._

  val boardState: Array[Array[Char]] = Array.fill[Char](
    BOARD_HEIGHT,
    BOARD_WIDTH)(' ') // Initialize to empty space character
  var currentPiece: Piece = Piece.randomPiece()
  var currentTop: Int = _
  var currentLeft: Int = _

  setCurrentTopAndLeft()

  def gameOver: Boolean = {
    val blocks = for {
      i <- 0 until 4
      j <- 0 until BOARD_WIDTH
    } yield boardState(i)(j) == '*'

    val hasCollision = {
      val status = for {
        i <- 0 until currentPiece.sideLength
        j <- 0 until currentPiece.sideLength

        x = currentTop + i
        y = currentLeft + j
      } yield currentPiece.state(i)(j) == '*' && boardState(x)(y) == '*'

      status.exists(identity)
    }

    blocks.exists(identity) || hasCollision
  }

  def setCurrentTopAndLeft() = {
    if (currentPiece.sideLength == 2) {
      currentTop = 2
      currentLeft = 9
    } else if (currentPiece.sideLength == 3) {
      currentTop = 1
      currentLeft = 9
    } else {
      currentTop = 0
      currentLeft = 8
    }
  }

  def fallOneRow() = {
    currentTop = currentTop + 1
  }

  def printBoard() = {
    val temporaryBoard = boardState.map(_.clone())

    for {
      i <- 0 until currentPiece.sideLength
      j <- 0 until currentPiece.sideLength

      x = currentTop + i
      y = currentLeft + j
      if y >= 0 && y < BOARD_WIDTH && x >= 0 && x < BOARD_HEIGHT && temporaryBoard(
        x)(y) == ' '
    } {
      temporaryBoard(x)(y) = currentPiece.state(i)(j)
    }

    for {
      i <- 4 until BOARD_HEIGHT
    } {
      println(('*' +: temporaryBoard(i) :+ '*').mkString)
    }

    println("*" * (BOARD_WIDTH + 2))
  }

  def sticked: Boolean = {
    val isAtBottom = {
      var i = currentPiece.sideLength - 1

      while (i >= 0 && !currentPiece.state(i).contains('*')) {
        i -= 1
      }

      currentTop + i + 1 == BOARD_HEIGHT
    }

    if (isAtBottom) return true

    val stickedToOtherPiece = {
      val arr = for {
        i <- 0 until currentPiece.sideLength
        j <- 0 until currentPiece.sideLength

        belowX = currentTop + i + 1
        belowY = currentLeft + j
        if belowY >= 0 && belowY < BOARD_WIDTH
      } yield {
        if (currentPiece.state(i)(j) == '*' && boardState(belowX)(belowY) == '*') {
          true
        } else {
          false
        }
      }

      arr.exists(identity)
    }

    stickedToOtherPiece
  }

  def updateState() = {
    // Put the current piece to the board
    updateBoard()

    // Clear the lines starting from the bottom (if there are such lines)
    clearLines()

    // Spawn a random piece
    currentPiece = Piece.randomPiece()

    // Update the current top and left index of the current falling piece
    setCurrentTopAndLeft()

    // Exit the game if over
    if (gameOver) {
      println(s"GAME OVER!")
      System.exit(1)
    }
  }

  def clearLines() = {
    var currLine = BOARD_HEIGHT - 1
    var lineClear = boardState(currLine).forall(_ == '*')

    while (currLine > 0) {
      if (lineClear) {
        for {
          i <- (4 until currLine).reverse
          j <- 0 until BOARD_WIDTH
        } {
          boardState(i + 1)(j) = boardState(i)(j)
        }
      }

      lineClear = boardState(currLine).forall(_ == '*')

      while (!lineClear && currLine > 0) {
        currLine -= 1
        lineClear = boardState(currLine).forall(_ == '*')
      }
    }
  }

  def updateBoard() = {
    for {
      i <- 0 until currentPiece.sideLength
      j <- 0 until currentPiece.sideLength

      x = currentTop + i
      y = currentLeft + j
      if y >= 0 && y < BOARD_WIDTH && x >= 0 && x < BOARD_HEIGHT && boardState(
        x)(y) == ' '
    } {
      boardState(x)(y) = currentPiece.state(i)(j)
    }
  }

  def tick() = {
    // Check if the piece can't be moved anymore
    if (sticked) {
      updateState()
    }

    fallOneRow()
    printBoard()

    ()
  }

  def start() = {
    var valid = true

    while (true) {
      if (valid) {
        tick()
        print(s"Enter move: ")
      } else {
        print(s"Invalid move. Please enter a new move: ")
      }

      StdIn.readLine() match {
        case "a" => valid = currentPiece.moveLeft(this)
        case "d" => valid = currentPiece.moveRight(this)
        case "w" => valid = currentPiece.rotateCounterClockwise(this)
        case "s" => valid = currentPiece.rotateClockwise(this)
        case _   => valid = true
      }
    }
  }
}

object Tetris {
  val BOARD_HEIGHT = 24
  val BOARD_WIDTH = 20

  def main(args: Array[String]): Unit = {
    val game = new Tetris
    game.start()
  }
}
