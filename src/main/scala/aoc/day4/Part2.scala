package aoc
package day4

import scala.annotation.tailrec
import scala.collection.mutable

object Part2 extends Challenge(day(4)):
  def execute =
    val cards = input.map(Card.parse)

    val cache = mutable.Map.empty[Int, Int]

    def processCard(indexes: List[Int], count: Int): Int = {
      val (i, pending) = (indexes.head, indexes.tail)
      if cache.contains(i) && pending.isEmpty
      then count + cache(i)
      else if cache.contains(i)
      then processCard(pending, count + cache(i))
      else
        val card = cards(i)
        val matchCount = card.matchCount
        val remainingCards = cards.length - i - 1

        val nextCount = matchCount min remainingCards
        val nextIndexes = Range(i + 1, i + nextCount).inclusive.toList
        val totalIndexes = pending ++ nextIndexes
        if totalIndexes.nonEmpty then
          val result = processCard(totalIndexes, count + 1)
          cache.update(i, result)
          result
        else
          cache.update(i, count + 1)
          count + 1
    }

    (0 to cards.length - 1).reverse.map(i => processCard(List(i), 0)).sum
