package aoc
package day1

object Part2 extends Challenge(day(1).part(2)):
  val numberMap = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  private def toNumber(input: String) =
    numberMap.get(input) match
      case Some(number) => number
      case None         => input.toInt

  def execute =
    val part = numberMap.keys.mkString("|")
    val reg = raw"(?<=(\d|$part))".r
    input.foldLeft(0) { (acc, line) =>
      val matches = reg.findAllMatchIn(line).toList

      if matches == Nil then throw Exception(s"Invalid line: $line")
      else
        val head = toNumber(matches.head.group(1).toString)
        val tail = toNumber(matches.reverse.head.group(1).toString)
        val concatinated = head.toString + tail.toString
        acc + concatinated.toInt
    }
