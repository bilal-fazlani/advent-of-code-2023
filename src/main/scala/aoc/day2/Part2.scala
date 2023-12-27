package aoc
package day2

object Part2 extends ChallengeSync(day(2).part(2)):

  def parseLine(line: String): Game =
    Game.parse(line).orDie(_.toString)

  def execute: Int =
    def colorMax(game: Game, color: Color) = game.reveals
      .flatMap(_.counts.collect { case CubeCount(count, `color`) =>
        count
      })
      .max

    input
      .map(parseLine)
      .map { game =>
        val redMax = colorMax(game, Color.Red)
        val blueMax = colorMax(game, Color.Blue)
        val greenMax = colorMax(game, Color.Green)
        redMax * blueMax * greenMax
      }
      .sum
