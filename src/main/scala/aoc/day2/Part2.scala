package aoc
package day2

object Part2 extends Challenge(day(2).part(2)):
  def execute =
    def colorMax(game: Game, color: Color) = game.reveals
      .flatMap(_.counts.collect { case CubeCount(count, `color`) =>
        count
      })
      .max

    input
      .map(Game.parse)
      .map { game =>
        val redMax = colorMax(game, Color.Red)
        val blueMax = colorMax(game, Color.Blue)
        val greenMax = colorMax(game, Color.Green)
        redMax * blueMax * greenMax
      }
      .sum
