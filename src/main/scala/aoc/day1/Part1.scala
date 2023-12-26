package aoc
package day1

import zio.*

object Part1 extends Challenge[Int](day(1).part(1)):
  def execute: Task[Int] =
    val reg = raw"[\d]".r
    file.runFoldZIO(0) { (acc, line) =>
      val matches = reg.findAllMatchIn(line).toList

      if matches == Nil then ZIO.fail(Exception(s"Invalid line: $line"))
      else
        val head = matches.head.toString
        val tail = matches.reverse.head.toString
        val concatinated = head + tail
        ZIO.succeed(acc + concatinated.toInt)
    }
