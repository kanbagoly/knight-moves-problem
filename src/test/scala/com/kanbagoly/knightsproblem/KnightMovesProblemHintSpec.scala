package com.kanbagoly.knightsproblem


import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KnightMovesProblemHintSpec extends AnyWordSpec with Matchers {

  "Solution" when {
    "the path length is 10" should {
      "contain all the digits of 1, 3, 8, 9 at least once" in {
        val result = KnightMovesProblem.solve(10)
        result.toString should contain allOf('1', '3', '8', '9')
      }
    }
  }

}
