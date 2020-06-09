package com.kanbagoly.knightsproblem

import com.kanbagoly.knightsproblem.KeyPad.{buttonIndices, isKey, isVowel, keyId}

import scala.collection.mutable

class KnightMovesProblem private (maxLength: Int) {

  import KnightMovesProblem._

  def solve(): Long = {
    val memo = mutable.HashMap.empty[(Int, Int, Int), Long]
    buttonIndices.map{case (x, y) => compute(x, y, 0, 0, memo)}.sum
  }

  def compute(row: Int, column: Int, length: Int, vowels: Int, memo: mutable.HashMap[(Int, Int, Int), Long]): Long =
    if (length >= maxLength) 0
    else {
      val actualVowels = vowels + (if (isVowel(row, column)) 1 else 0)
      if (actualVowels > MaxVowels) 0
      else memo.getOrElseUpdate((keyId(row, column), length, actualVowels),
        KnightSteps
          .map { case (x, y) => (row + x, column + y) }
          .collect { case (x, y) if isKey(x, y) => compute(x, y, length + 1, actualVowels, memo) }
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
