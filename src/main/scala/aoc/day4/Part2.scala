package aoc
package day4

import scala.annotation.tailrec
import scala.collection.mutable

object Part2 extends Challenge(day(4)):
  def execute =
    val cards = input.map(Card.parse)

    val cache = mutable.Map.empty[Int, Int]

    @tailrec
    def processCard(i: Int, pendingIndexes: List[Int], count: Int): Int = {
      cache.get(i) match
        case None =>
          val card = cards(i)
          val matchCount = card.matchCount

          val nextCount = matchCount min (cards.length - 1 - i)
          val nextIndexes = Range(i + 1, i + nextCount).inclusive.toList
          val totalIndexes = pendingIndexes ++ nextIndexes
          if totalIndexes.nonEmpty
          then processCard(totalIndexes.head, totalIndexes.tail, count = count + 1)
          else count + 1
        case Some(cached) => count + cached
    }

    Range(0, cards.length).map { i =>
      val out = processCard(i, Nil, 0)
      cache.update(i, out)
      out
    }.sum
