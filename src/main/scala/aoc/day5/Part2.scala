package aoc
package day5

import MapLine.MapTitle
import MapElement.*
import aoc.day5.Syntax.mapping
import scala.collection.immutable.NumericRange
import scala.io.Source
import zio.stream.ZStream
import zio.stream.ZChannel
import zio.Chunk
import scala.util.control.Breaks.*

object Part2 extends Challenge(day(5)):
  def execute =
    val seeds = Seeds.parse(input.head)
    val mappings = Mapping.constructAll(input.tail)
    val flattenned = seeds.grouped(2).map { case head :: next :: Nil =>
      range2(head, next)
    }

    var outerMin: Option[Long] = None
    breakable {
      for (i1 <- flattenned) {
        var innerMin: Option[Long] = None
        breakable {
          for (i2 <- i1) {
            break
          }
        }
      }
    }

    outerMin.get

    // var minlocation: Option[Long] = None
    flattenned
      .scanLeft(-999L) { (cmin, seed) =>
        val location = Mapping.resolve(Seed, Location, seed)(mappings)
        if cmin == -999L then location else cmin min location
      }
      .zipWithIndex
      .foreach { case (min, i) =>
        print(s"\r[$i]: $min")
      }

    0
    // val locations = flattenned.map(seed => Mapping.resolve(Seed, Location, seed)(mappings))
    // locations.min

def range(start: Long, n: Long) = new Iterable[Long] {
  override val iterator: Iterator[Long] = new Iterator[Long] {
    var nxt: Option[Long] = Some(start)
    override def hasNext: Boolean = {
      nxt != None
    }

    override def next(): Long = {
      nxt.fold(throw IndexOutOfBoundsException(s"$nxt: out of range")) { r =>
        if r + 1 == (start + n) then nxt = None else nxt = Some(r + 1)
        r
      }
    }
  }
}

def range2(start: Long, n: Long): Iterator[Long] = new Iterator[Long] {
  var nxt: Option[Long] = Some(start)
  override def hasNext: Boolean = {
    nxt != None
  }

  override def next(): Long = {
    nxt.fold(throw IndexOutOfBoundsException(s"$nxt: out of range")) { r =>
      if r + 1 == (start + n) then nxt = None else nxt = Some(r + 1)
      r
    }
  }
}

@main def testing = {
  val r = range(15, 2)
  println(r.toList)
}
