package aoc
package day4

import scala.annotation.tailrec

object Part2 extends Challenge(day(4)):
  def execute =
    val cards = input.map(Card.parse)

    var count = 0

    @tailrec
    def processCard(i: Int, pendingIndexes: List[Int]): Unit = {
      count += 1
      val card = cards(i)
      val matchCount = card.matchCount

      val nextCount = matchCount min (cards.length - 1 - i)
      val nextIndexes = Range(i + 1, i + nextCount).inclusive.toList
      val totalIndexes = pendingIndexes ++ nextIndexes
      if totalIndexes.nonEmpty
      then processCard(totalIndexes.head, totalIndexes.tail)
    }

    for (i <- 0 to cards.length - 1) do processCard(i, Nil)
    count
