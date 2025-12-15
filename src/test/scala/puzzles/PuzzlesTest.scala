package puzzles

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import puzzles.day1.{Day1Part1, Day1Part2, Day1Part2_optimised}
import puzzles.day2.{Day2Part1, Day2Part2}

class PuzzlesTest extends AnyFunSuite with Matchers {

  test("Day1Part1") {
    val input  = Utils.getInputFromDay("day1")
    val actual = Day1Part1.getAns(input)

    actual should ===(1034)
  }

  test("Day1Part2") {
    val input  = Utils.getInputFromDay("day1")
    val actual = Day1Part2.getAns(input)

    actual should ===(6166)
  }

  test("Day1Part2_optimised") {
    val input  = Utils.getInputFromDay("day1")
    val actual = Day1Part2_optimised.getAns(input)

    actual should ===(6166)
  }

  test("Day2Part1") {
    val input  = Utils.getInputFromDay("day2")
    val actual = Day2Part1.getAns(input.head.split(',').toList)

    actual should ===(31000881061L)
  }

  test("Day2Part2") {
    val input  = Utils.getInputFromDay("day2")
    val actual = Day2Part2.getAns(input.head.split(',').toList)

    actual should ===(46769308485L)
  }
}
