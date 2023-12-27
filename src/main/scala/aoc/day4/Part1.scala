package aoc
package day4

object Part1 extends ChallengeSync(day(4)):
  def execute: Int = 
    input
      .map(Syntax.card.parseString(_).orDie(_.toString))
      .map(calculatePoints)
      .sum

  def calculatePoints(card: Card): Int =
    val n = card.winningNumbers.intersect(card.ownNumbers).size
    Math.pow(2, n - 1).toInt
