package aoc
package day2

object Part1 extends Challenge(day(2).part(1)):

  val maxRed = 12
  val maxGreen = 13
  val maxBlue = 14

  def execute =
    input
      .map(Game.parse)
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
      .sum
