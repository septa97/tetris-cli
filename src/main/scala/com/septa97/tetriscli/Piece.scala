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
