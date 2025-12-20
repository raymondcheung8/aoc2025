package puzzles

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import puzzles.day1.{Day1Part1, Day1Part2, Day1Part2v2}
import puzzles.day2.{Day2Part1, Day2Part2, Day2Part2v2, Day2Part2v3, Day2Part2v4}
import puzzles.day3.{Day3Part1, Day3Part2}
import puzzles.day4.{Day4Part1, Day4Part2}
import puzzles.day5.{Day5Part1, Day5Part2}
import puzzles.day6.{Day6Part1, Day6Part2}
import puzzles.day7.{Day7Part1, Day7Part2}
import puzzles.day8.{Day8Part1, Day8Part2}

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

    "Part 2 (recursing every time it goes past 0)" ignore {
      Day1Part2.getAns(input) shouldEqual answers((day, "part2"))
    }

    "Part 2 v2 (using floorMod and floorDiv)" in {
      Day1Part2v2.getAns(input) shouldEqual answers((day, "part2"))
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

    "Part 2 (using factors and partitioning each string)" ignore {
      Day2Part2.getAns(input) shouldEqual answers((day, "part2"))
    }

    "Part 2 v2 (using substring lengths and partitioning each string)" ignore {
      Day2Part2v2.getAns(input) shouldEqual answers((day, "part2"))
    }

    "Part 2 v3 (using factors and duplicating substring)" ignore {
      Day2Part2v3.getAns(input) shouldEqual answers((day, "part2"))
    }

    "Part 2 v4 (using substring lengths and duplicating substring)" in {
      Day2Part2v4.getAns(input) shouldEqual answers((day, "part2"))
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
    val day          = "day6"
    val input        = Utils.getInputFromDay(day)
    val exampleInput = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ".split('\n').toList

    "Part 1 example" in {
      Day6Part1.getAns(exampleInput) shouldEqual 4277556L
    }

    "Part 1" in {
      Day6Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day6Part2.getAns(exampleInput) shouldEqual 3263827L
    }

    "Part 2" in {
      Day6Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 7" should {
    val day          = "day7"
    val input        = Utils.getInputFromDay(day)
    val exampleInput =
      ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."
        .split('\n')
        .toList

    "Part 1 example" in {
      Day7Part1.getAns(exampleInput) shouldEqual 21
    }

    "Part 1" in {
      Day7Part1.getAns(input) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day7Part2.getAns(exampleInput) shouldEqual 40
    }

    "Part 2" in {
      Day7Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }

  "Day 8" should {
    val day          = "day8"
    val input        = Utils.getInputFromDay(day)
    val exampleInput =
      "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
        .split('\n')
        .toList

    "Part 1 example" in {
      Day8Part1.getAns(exampleInput, 10) shouldEqual 40
    }

    "Part 1" in {
      Day8Part1.getAns(input, 1000) shouldEqual answers((day, "part1"))
    }

    "Part 2 example" in {
      Day8Part2.getAns(exampleInput) shouldEqual 25272
    }

    "Part 2" in {
      Day8Part2.getAns(input) shouldEqual answers((day, "part2"))
    }
  }
}
