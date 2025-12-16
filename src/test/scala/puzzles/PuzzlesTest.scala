package puzzles

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import puzzles.day1.{Day1Part1, Day1Part2, Day1Part2_optimised}
import puzzles.day2.{Day2Part1, Day2Part2}
import puzzles.day3.Day3Part1

class PuzzlesTest extends AnyWordSpec with Matchers {

  "Day 1" should {
    val input = Utils.getInputFromDay("day1")
    val exampleInput = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82".split('\n').toList

    "Part 1 example" in {
      val actual = Day1Part1.getAns(exampleInput)
      actual shouldEqual 3
    }

    "Part 1" in {
      Day1Part1.getAns(input) shouldEqual 1034
    }

    "Part 2 example" in {
      Day1Part2.getAns(exampleInput) shouldEqual 6
    }

    "Part 2" in {
      Day1Part2.getAns(input) shouldEqual 6166
    }

    "Part 2 optimised" in {
      Day1Part2_optimised.getAns(input) shouldEqual 6166
    }
  }

  "Day 2" should {
    val input = Utils.getInputFromDay("day2").flatMap(_.split(',').toList)
    val exampleInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124".split(',').toList

    "Part 1 example" in {
      Day2Part1.getAns(exampleInput) shouldEqual 1227775554L
    }

    "Part 1" in {
      Day2Part1.getAns(input) shouldEqual 31000881061L
    }

    "Part 2 example" in {
      Day2Part2.getAns(exampleInput) shouldEqual 4174379265L
    }

    "Part 2" in {
      Day2Part2.getAns(input) shouldEqual 46769308485L
    }
  }

  "Day 3" should {
    val input = Utils.getInputFromDay("day3").flatMap(_.split(',').toList)
    val exampleInput = "987654321111111\n811111111111119\n234234234234278\n818181911112111".split('\n').toList

    "Part 1 example" in {
      Day3Part1.getAns(exampleInput) shouldEqual 357
    }
  }
}
