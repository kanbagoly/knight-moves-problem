package com.kanbagoly.knightsproblem

import com.kanbagoly.knightsproblem.KeyPad.{keyIndices, isKey, isVowel}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

private object KeyPad {

  private sealed trait Key
  private case object Vowel extends Key
  private case object Consonant extends Key
  private case object Empty extends Key

  private val Keys: ArraySeq[ArraySeq[Key]] = ArraySeq(
    ArraySeq(Vowel, Consonant, Consonant, Consonant, Vowel),
    ArraySeq(Consonant, Consonant, Consonant, Vowel, Consonant),
    ArraySeq(Consonant, Consonant, Consonant, Consonant, Vowel),
    ArraySeq(Empty, Consonant, Consonant, Consonant, Empty)
  )

  private val Rows = Keys.size
  private val Columns = Keys.head.size

  def keyIndices: Seq[(Int, Int)] =
    for {
      x <- Keys.indices
      y <- Keys(x).indices
      if Keys(x)(y) != Empty
    } yield (x ,y)

  def isKey(row: Int, column: Int): Boolean =
    0 <= row && row < Rows &&
      0 <= column && column < Columns &&
      Keys(row)(column) != Empty

  def isVowel(row: Int, column: Int): Boolean = Keys(row)(column) == Vowel

}

object KnightMovesProblem {

  def solve(maxLength: Int): Long = solve(maxLength, mutable.HashMap.empty)

  private def solve(maxLength: Int, memo: mutable.HashMap[(Int, Int, Int, Int), Long]): Long = {
    def compute(row: Int, column: Int, length: Int = 0, vowels: Int = 0): Long =
      if (length >= maxLength) 0
      else {
        val actualVowels = vowels + (if (isVowel(row, column)) 1 else 0)
        if (actualVowels > MaxVowels) 0
        else memo.getOrElseUpdate((row, column, length, actualVowels),
          KnightSteps
            .map { case (x, y) => (row + x, column + y) }
            .collect { case (x, y) if isKey(x, y) => compute(x, y, length + 1, actualVowels) }
            .sum
            .max(1))
      }
    keyIndices.map { case (x, y) => compute(x, y) }.sum
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
