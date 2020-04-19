package com.kanbagoly.knightsproblem

import scala.collection.immutable.ArraySeq

sealed trait Key
case object Vowel extends Key
case object Consonant extends Key
case object Empty extends Key

private object KeyPad {

  val Buttons: ArraySeq[ArraySeq[Key]] = ArraySeq(
    ArraySeq(Vowel, Consonant, Consonant, Consonant, Vowel),
    ArraySeq(Consonant, Consonant, Consonant, Vowel, Consonant),
    ArraySeq(Consonant, Consonant, Consonant, Consonant, Vowel),
    ArraySeq(Empty, Consonant, Consonant, Consonant, Empty)
  )

  private val Rows = Buttons.size
  private val Columns = Buttons.head.size

  def isKey(row: Int, column: Int): Boolean =
    0 <= row && row < Rows &&
      0 <= column && column < Columns &&
      Buttons(row)(column) != Empty

  def keyId(row: Int, column: Int): Int = row * Columns + column

}
