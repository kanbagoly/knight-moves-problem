package com.kanbagoly.knightsproblem

import com.kanbagoly.knightsproblem.KeyPad.{buttonIndices, isKey, isVowel, keyId}

import scala.collection.mutable

object KnightMovesProblem {

  def solve(maxLength: Int): Long =
    solve(maxLength, mutable.HashMap.empty[(Int, Int, Int), Long])

  private def solve(maxLength: Int, memo: mutable.HashMap[(Int, Int, Int), Long]): Long = {
    def compute(row: Int, column: Int, length: Int, vowels: Int): Long =
      if (length >= maxLength) 0
      else {
        val actualVowels = vowels + (if (isVowel(row, column)) 1 else 0)
        if (actualVowels > MaxVowels) 0
        else memo.getOrElseUpdate((keyId(row, column), length, actualVowels),
          KnightSteps
            .map { case (x, y) => (row + x, column + y) }
            .collect { case (x, y) if isKey(x, y) => compute(x, y, length + 1, actualVowels) }
            .sum
            .max(1))
      }
    buttonIndices.map{case (x, y) => compute(x, y, 0, 0)}.sum
  }

  private val MaxVowels = 2

  private val KnightSteps: Seq[(Int, Int)] = {
    val steps = for {
      x <- Seq(1, -1)
      y <- Seq(2, -2)
    } yield (x, y)
    steps ++ steps.map(_.swap)
  }

}
