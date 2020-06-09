package com.kanbagoly.knightsproblem

import scala.collection.immutable.ArraySeq

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

  def isVowel(row: Int, column: Int) = Buttons(row)(column) == Vowel

  def keyId(row: Int, column: Int): Int = row * Columns + column

}
