package com.kanbagoly.knightsproblem


import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KnightMovesProblemSpec extends AnyWordSpec with Matchers {

  "Solution" when {
    "the path length is 0" should {
      "be 0" in {
        val result = KnightMovesProblem.solve(0)
        result should be(0)
      }
    }
    "the path length is 1" should {
      "be the number of keys" in {
        val result = KnightMovesProblem.solve(1)
        result should be(18)
      }
    }
    "the path length is 10" should {
      "contain all the digits of 1, 3, 8, 9 at least once" in {
        val result = KnightMovesProblem.solve(10)
        result.toString should contain allOf('1', '3', '8', '9')
      }
    }
  }

}
