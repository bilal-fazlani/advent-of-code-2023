package aoc
package day4

import zio.*

object Part1 extends Challenge[Int](day(4)):
  import Syntax.*
  def execute =
    file
      .map(card.parseString)
      .absolve
      .mapError(e => Exception(e.toString))
      .map(calculatePoints)
      .runSum

  def calculatePoints(card: Card): Int =
    val n = card.winningNumbers.intersect(card.ownNumbers).size
    Math.pow(2, n - 1).toInt
