package aoc
package day1

object Part1 extends Challenge(day(1).part(1)):
  def execute =
    val reg = raw"[\d]".r
    input.foldLeft(0) { (acc, line) =>
      val matches = reg.findAllMatchIn(line).toList
      if matches == Nil then throw Exception(s"Invalid line: $line")
      else
        val head = matches.head.toString
        val tail = matches.reverse.head.toString
        val concatinated = head + tail
        acc + concatinated.toInt
    }
