package com.kanbagoly.knightsproblem

import com.kanbagoly.knightsproblem.KeyPad.{buttonIndices, isKey, isVowel, keyId}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

private object KeyPad {

  private sealed trait Key
  private case object Vowel extends Key
  private case object Consonant extends Key
  private case object Empty extends Key

  private val Buttons: ArraySeq[ArraySeq[Key]] = ArraySeq(
    ArraySeq(Vowel, Consonant, Consonant, Consonant, Vowel),
    ArraySeq(Consonant, Consonant, Consonant, Vowel, Consonant),
    ArraySeq(Consonant, Consonant, Consonant, Consonant, Vowel),
    ArraySeq(Empty, Consonant, Consonant, Consonant, Empty)
  )

  private val Rows = Buttons.size
  private val Columns = Buttons.head.size

  def buttonIndices: Seq[(Int, Int)] =
    for {
      x <- Buttons.indices
      y <- Buttons(x).indices
      if Buttons(x)(y) != Empty
    } yield (x ,y)

  def isKey(row: Int, column: Int): Boolean =
    0 <= row && row < Rows &&
      0 <= column && column < Columns &&
      Buttons(row)(column) != Empty

  def isVowel(row: Int, column: Int): Boolean = Buttons(row)(column) == Vowel

  def keyId(row: Int, column: Int): Int = row * Columns + column

}

object KnightMovesProblem {

  def solve(maxLength: Int): Long = solve(maxLength, mutable.HashMap.empty)

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
