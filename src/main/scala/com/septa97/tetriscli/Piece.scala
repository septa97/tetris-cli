package com.septa97.tetriscli

import scala.util.Random

case class Piece(pieceType: Char) {
  var state: Array[Array[Char]] = pieceType match {
    case 'I' => Piece.I
    case 'L' => Piece.L
    case 'J' => Piece.J
    case 'Z' => Piece.Z
    case 'O' => Piece.O
  }
  val sideLength: Int = state.length

  def rotateClockwise(tetris: Tetris): Boolean = {
    val newState: Array[Array[Char]] = Array.ofDim[Char](sideLength, sideLength)

    for {
      i <- 0 until sideLength
      j <- 0 until sideLength
    } {
      newState(i)(j) = state(sideLength - j - 1)(i)
    }

    val isValid = hasCollision(tetris, newState)
    if (isValid) state = newState

    isValid
  }

  def rotateCounterClockwise(tetris: Tetris): Boolean = {
    val newState: Array[Array[Char]] = Array.ofDim[Char](sideLength, sideLength)

    for {
      i <- 0 until sideLength
      j <- 0 until sideLength
    } {
      newState(i)(j) = state(j)(sideLength - i - 1)
    }

    val isValid = hasCollision(tetris, newState)
    if (isValid) state = newState

    isValid
  }

  def moveLeft(tetris: Tetris): Boolean = {
    val valid = (tetris.currentLeft > 0 || tetris.currentPiece.state
      .map(e => e(math.abs(tetris.currentLeft)) == ' ')
      .forall(identity)) && !hasFutureCollision(tetris,
                                                tetris.currentLeft - 1,
                                                tetris.currentTop + 1)

    if (valid) {
      tetris.currentLeft -= 1
    }

    valid
  }

  def moveRight(tetris: Tetris): Boolean = {
    val valid = (tetris.currentLeft + tetris.currentPiece.sideLength < Tetris.BOARD_WIDTH || tetris.currentPiece.state
      .map(e => e(Tetris.BOARD_WIDTH - tetris.currentLeft - 1) == ' ')
      .forall(identity)) && !hasFutureCollision(tetris,
                                                tetris.currentLeft + 1,
                                                tetris.currentTop + 1)

    if (valid) {
      tetris.currentLeft += 1
    }

    valid
  }

  def hasCollision(tetris: Tetris, newState: Array[Array[Char]]): Boolean = {
    val valids = for {
      i <- 0 until sideLength
      j <- 0 until sideLength

      x = tetris.currentTop + i
      y = tetris.currentLeft + j
    } yield {
      if (newState(i)(j) == '*') {
        if (y >= 0 && tetris.boardState(x)(y) == ' ') {
          true
        } else {
          false
        }
      } else {
        true
      }
    }

    valids.forall(identity)
  }

  def hasFutureCollision(tetris: Tetris,
                         futureLeft: Int,
                         futureTop: Int): Boolean = {
    val collisions = for {
      i <- 0 until tetris.currentPiece.sideLength
      j <- 0 until tetris.currentPiece.sideLength

      x = futureTop + i
      y = futureLeft + j
      if x >= 0 && x < Tetris.BOARD_HEIGHT && y >= 0 && y < Tetris.BOARD_WIDTH
    } yield {
      if (tetris.boardState(x)(y) == '*' && tetris.currentPiece.state(i)(j) == '*')
        true
      else false
    }

    collisions.exists(identity)
  }

}

object Piece {
  val pieces = Array('I', 'L', 'J', 'Z', 'O')

  val I = Array(
    Array(' ', ' ', ' ', ' '),
    Array(' ', ' ', ' ', ' '),
    Array(' ', ' ', ' ', ' '),
    Array('*', '*', '*', '*')
  )

  val L = Array(
    Array('*', ' ', ' '),
    Array('*', ' ', ' '),
    Array('*', '*', ' ')
  )

  val J = Array(
    Array(' ', ' ', '*'),
    Array(' ', ' ', '*'),
    Array(' ', '*', '*')
  )

  val Z = Array(
    Array(' ', ' ', '*'),
    Array(' ', '*', '*'),
    Array(' ', '*', ' ')
  )

  val O = Array(
    Array('*', '*'),
    Array('*', '*')
  )

  def randomPiece(): Piece = Piece(pieces(Random.nextInt(pieces.length)))
}
