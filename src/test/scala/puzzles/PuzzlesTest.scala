package puzzles

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import puzzles.day1.{Day1Part1, Day1Part2, Day1Part2_optimised}
import puzzles.day2.{Day2Part1, Day2Part2}

class PuzzlesTest extends AnyWordSpec with Matchers {

  "Day 1" should {
    "Part 1" in {
      val input = Utils.getInputFromDay("day1")
      val actual = Day1Part1.getAns(input)

      actual should ===(1034)
    }

    "Part 2" in {
      val input = Utils.getInputFromDay("day1")
      val actual = Day1Part2.getAns(input)

      actual should ===(6166)
    }

    "Part 2 optimised" in {
      val input = Utils.getInputFromDay("day1")
      val actual = Day1Part2_optimised.getAns(input)

      actual should ===(6166)
    }
  }

  "Day 2" should {
    "Part 1" in {
      val input = Utils.getInputFromDay("day2")
      val actual = Day2Part1.getAns(input.head.split(',').toList)

      actual should ===(31000881061L)
    }

    "Part 2" in {
      val input = Utils.getInputFromDay("day2")
      val actual = Day2Part2.getAns(input.head.split(',').toList)

      actual should ===(46769308485L)
    }
  }
}
