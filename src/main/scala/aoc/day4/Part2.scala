package aoc
package day4

object Part2 extends Challenge(day(4)):
  def execute =
    val cards = input.map(Card.parse)

    def process(card: Card, index: Int, cache: Map[Int, Int]): Map[Int, Int] =
      val matchCount = card.matchCount
      val remainingCards = cards.length - index - 1

      val nextCount = matchCount min remainingCards
      val nextIndexes = Range(index + 1, index + nextCount).inclusive.toList

      val theirCount = nextIndexes.map(cache(_)).sum
      cache.updated(index, theirCount + 1)

    cards.zipWithIndex
      .foldRight((Map.empty[Int, Int])) { case ((card, index), cache) =>
        process(card, index, cache)
      }
      .map(_._2)
      .sum
