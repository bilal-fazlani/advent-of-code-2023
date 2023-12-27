package aoc
package day4

import zio.*

object Part1 extends Challenge[Int](day(4)):
  def execute = for {
    lines <- file.runCollect
    cards = ???
  } yield 0
