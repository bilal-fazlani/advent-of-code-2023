package aoc
package day2

import zio.*
import zio.stream.*
import aoc.Challenge

object Day2 extends Challenge[Int](day(2)):

  def execute: Task[Int] =
    ZIO.succeed(0)

enum Color:
  case Blue, Red, Green
case class CubeCount(count: Int, color: Color)
case class Reveal(counts: Set[CubeCount])
case class Game(id: Int, reveals: Seq[Reveal])

object GameSyntax {
  import zio.parser.*
  import zio.parser.Parser.*

  val comma = char(',')
  val semicolon = char(';')
  def number = digit.repeat.map(_.mkString.toInt)
  def color = string("blue", Color.Blue) | string("red", Color.Red) | string(
    "green",
    Color.Green
  )
  def cubeCount =
    ((number zipLeft whitespace.repeat) ~ color).map(CubeCount.apply)
  def reveal = cubeCount
    .repeatWithSep(comma zipLeft whitespace.optional)
    .map(x => Reveal(x.toSet))
  def game =
    ((string("Game", ()) ~ whitespace.unit ~ number ~ string(
      ":",
      ()
    ) ~ whitespace.unit)
      ~ (reveal.repeatWithSep(semicolon zipLeft whitespace.optional)))
      .map { case (id, reveals) =>
        Game(id, reveals)
      }
}
