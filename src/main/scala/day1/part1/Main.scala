package day1.part1

import zio.*
import zio.Console.*
import zio.stream.*

object Main extends ZIOAppDefault:
  def run =
    val input = ZStream
      .fromFileName("src/main//scala/day1/part1/input.txt")
      .via(ZPipeline.utfDecode)
      .via(ZPipeline.splitLines)
    Part1.calculate(input).flatMap(printLine(_)).exitCode
