package aoc
package day2

import zio.*
import zio.stream.*
import aoc.Challenge

object Day2 extends Challenge[Seq[Game]](day(2)):

  def parseLine(line: String) =
    Game.parse(line)

  def execute: Task[Seq[Game]] =
    file
      .map(parseLine)
      .absolve
      .runCollect
      .mapError(e => Exception(e.toString))

enum Color:
  case Blue, Red, Green
case class CubeCount(count: Int, color: Color)
case class Reveal(counts: Set[CubeCount])
case class Game(id: Int, reveals: Seq[Reveal])
object Game:
  def parse(line: String) =
    import GameSyntax.*
    game.parseString(line)

object GameSyntax {
  import zio.parser.*
  import zio.parser.Parser.*

  lazy val comma = char(',')
  lazy val semicolon = char(';')

  lazy val number = digit.repeat.map(_.mkString.toInt)

  lazy val color =
    (string("blue", Color.Blue) | string("red", Color.Red) | string(
      "green",
      Color.Green
    )) ?? "color"

  lazy val cubeCount =
    ((number zip whitespace.optional.unit) ~ color).map(CubeCount.apply)

  lazy val reveal = cubeCount
    .repeatWithSep(comma zip whitespace.optional.unit)
    .map(x => Reveal(x.toSet))

  lazy val game =
    ((string("Game", ()) ~ whitespace.unit ~ number ~ string(
      ":",
      ()
    ) ~ whitespace.unit)
      ~ (reveal.repeatWithSep(semicolon zip whitespace.optional.unit)))
      .map { case (id, reveals) =>
        Game(id, reveals)
      } ~ end
}
