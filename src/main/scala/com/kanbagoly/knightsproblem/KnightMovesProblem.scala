package com.kanbagoly.knightsproblem

import com.kanbagoly.knightsproblem.KeyPad.{Buttons, isKey, keyId}

import scala.collection.mutable

class KnightMovesProblem(maxLength: Int) {

  import KnightMovesProblem._

  private val memory = mutable.Map[(Int, Int, Int), Long]()

  def solve(): Long = (
    for {
      x <- Buttons.indices
      y <- Buttons(x).indices
      if Buttons(x)(y) != Empty
    } yield compute(x, y)
    ).sum

  private def compute(row: Int, column: Int, length: Int = 0, vowels: Int = 0): Long =
    if (length >= maxLength) 0
    else {
      val actualVowels = vowels + (if (Buttons(row)(column) == Vowel) 1 else 0)
      if (actualVowels > MaxVowels) 0
      else memory.getOrElseUpdate((keyId(row, column), length, actualVowels),
        KnightSteps
          .map { case (x, y) => (row + x, column + y) }
          .collect { case (x, y) if isKey(x, y) => compute(x, y, length + 1, actualVowels) }
          .sum
          .max(1))
    }

}

object KnightMovesProblem {

  def solve(maxLength: Int): Long =
    new KnightMovesProblem(maxLength).solve()

  private val MaxVowels = 2

  private val KnightSteps: Seq[(Int, Int)] = {
    val steps = for {
      x <- Seq(1, -1)
      y <- Seq(2, -2)
    } yield (x, y)
    steps ++ steps.map(_.swap)
  }

}
