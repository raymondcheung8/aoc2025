package puzzles.day5

object Day5Part1 {
  def getAns(input: List[String]): Long = {
    val freshRanges =
      input.collect {
        case str if str.contains('-') =>
          str.split('-') match {
            case Array(l, r, _*) => (l.toLong, r.toLong)
          }
      }

    input.count { str =>
      str != "" &&
      !str.contains('-') &&
      freshRanges.exists { case (l, r) =>
        val x = str.toLong
        x >= l && x <= r
      }
    }
  }
}
