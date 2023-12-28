package aoc
package day4

import scala.annotation.tailrec

object Part2 extends Challenge(day(4)):
  def execute =
    val cards = input.map(Card.parse)

    @tailrec
    def processCard(indexes: List[Int], count: Int): Int = {
      val (i, pending) = (indexes.head, indexes.tail)
      val card = cards(i)
      val matchCount = card.matchCount
      val remainingCards = cards.length - i - 1

      val nextCount = matchCount min remainingCards
      if nextCount > 0 || pending.nonEmpty then
        val nextIndexes = Range(i + 1, i + nextCount).inclusive.toList
        val totalIndexes = pending ++ nextIndexes
        processCard(totalIndexes, count + 1)
      else count + 1
    }

    Range(0, cards.length).map(i => processCard(List(i), 0)).sum
