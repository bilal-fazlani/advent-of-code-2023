package day1.part1

import zio.*
import zio.stream.*
import scala.util.matching.Regex.Match

object Part1:
  def calculate(lines: ZStream[Any, Throwable, String]): Task[Int] =
    val reg = raw"[\d]".r
    lines.runFoldZIO(0) { (acc, line) =>
      val matches = reg.findAllMatchIn(line).toList

      if matches == Nil then ZIO.fail(Exception(s"Invalid line: $line"))
      else
        val head = matches.head.toString
        val tail = matches.reverse.head.toString
        val concatinated = head + tail
        ZIO.succeed(acc + concatinated.toInt)
    }
