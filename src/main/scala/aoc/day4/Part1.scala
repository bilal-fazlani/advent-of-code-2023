package aoc
package day4

object Part1 extends Challenge(day(4)):
  def execute: Int =
    input
      .map(Card.parse)
      .map(calculatePoints)
      .sum

  def calculatePoints(card: Card): Int =
    Math.pow(2, card.matchCount - 1).toInt
