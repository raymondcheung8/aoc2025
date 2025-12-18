package puzzles

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import puzzles.day1.{Day1Part1, Day1Part2, Day1Part2_optimised}
import puzzles.day2.{Day2Part1, Day2Part2}
import puzzles.day3.{Day3Part1, Day3Part2}
import puzzles.day4.{Day4Part1, Day4Part2}
import puzzles.day5.{Day5Part1, Day5Part2}
import puzzles.day6.Day6Part1

class PuzzlesTest extends AnyWordSpec with Matchers {
  val answers: Map[(String, String), Long] = Utils.getAnswers

  "Day 1" should {
    val day          = "day1"
    val input        = Utils.getInputFromDay(day)
    val exampleInput = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82".split('\n').toList

    "Part 1 example" in {
      val actual = Day1Part1.getAns(exampleInput)
      actual shouldEqual 3
    }

    "Part 1" in {
      Day1Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day1Part2.getAns(exampleInput) shouldEqual 6
    }

    "Part 2" in {
      Day1Part2.getAns(input) shouldEqual answers((day, "part2"))
    }

    "Part 2 optimised" in {
      Day1Part2_optimised.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 2" should {
    val day          = "day2"
    val input        = Utils.getInputFromDay(day)
    val exampleInput =
      "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" :: Nil

    "Part 1 example" in {
      Day2Part1.getAns(exampleInput) shouldEqual 1227775554L
    }

    "Part 1" in {
      Day2Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day2Part2.getAns(exampleInput) shouldEqual 4174379265L
    }

    "Part 2" in {
      Day2Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 3" should {
    val day          = "day3"
    val input        = Utils.getInputFromDay(day).flatMap(_.split(',').toList)
    val exampleInput = "987654321111111\n811111111111119\n234234234234278\n818181911112111".split('\n').toList

    "Part 1 example" in {
      Day3Part1.getAns(exampleInput) shouldEqual 357
    }

    "Part 1" in {
      Day3Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day3Part2.getAns(exampleInput) shouldEqual 3121910778619L
    }

    "Part 2" in {
      Day3Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 4" should {
    val day          = "day4"
    val input        = Utils.getInputFromDay(day)
    val exampleInput =
      "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.".split('\n').toList

    "Part 1 example" in {
      Day4Part1.getAns(exampleInput) shouldEqual 13
    }

    "Part 1" in {
      Day4Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day4Part2.getAns(exampleInput) shouldEqual 43
    }

    "Part 2" in {
      Day4Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 5" should {
    val day          = "day5"
    val input        = Utils.getInputFromDay(day)
    val exampleInput = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32".split('\n').toList

    "Part 1 example" in {
      Day5Part1.getAns(exampleInput) shouldEqual 3
    }

    "Part 1" in {
      Day5Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day5Part2.getAns(exampleInput) shouldEqual 14
    }

    "Part 2" in {
      Day5Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 6" should {
    val day = "day6"
    val input = Utils.getInputFromDay(day)
    val exampleInput = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ".split('\n').toList

    "Part 1 example" in {
      Day6Part1.getAns(exampleInput) shouldEqual 4277556L
    }

    "Part 1" in {
      Day6Part1.getAns(input) shouldEqual answers((day, "part1"))
    }
  }
}
