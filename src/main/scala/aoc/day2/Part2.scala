package aoc
package day2

import zio.*
import zio.stream.*

object Part2 extends Challenge[Int](day(2).part(2)):

  def parseLine(line: String) =
    Game.parse(line).left.map(e => Exception(e.toString))

  def execute: Task[Int] =
    def colorMax(game: Game, color: Color) = game.reveals
      .flatMap(_.counts.collect { case CubeCount(count, `color`) =>
        count
      })
      .max

    file
      .map(parseLine)
      .absolve
      .map { game =>
        val redMax = colorMax(game, Color.Red)
        val blueMax = colorMax(game, Color.Blue)
        val greenMax = colorMax(game, Color.Green)
        redMax * blueMax * greenMax
      }
      .runSum
      .mapError(e => Exception(e.toString))
