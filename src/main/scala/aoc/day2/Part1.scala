package aoc
package day2

import zio.*
import zio.stream.*

object Part1 extends Challenge[Int](day(2).part(1)):

  val maxRed = 12
  val maxGreen = 13
  val maxBlue = 14

  def parseLine(line: String) =
    Game.parse(line).left.map(e => Exception(e.toString))

  def execute: Task[Int] =
    file
      .map(parseLine)
      .absolve
      .filter(game =>
        game.reveals.forall(r =>
          r.counts.forall {
            case CubeCount(count, Color.Red)   => count <= maxRed
            case CubeCount(count, Color.Green) => count <= maxGreen
            case CubeCount(count, Color.Blue)  => count <= maxBlue
          }
        )
      )
      .map(_.id)
      .runSum
      .mapError(e => Exception(e.toString))
