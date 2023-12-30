package aoc
package day5

import MapLine.MapTitle
import MapElement.*
import aoc.day5.Syntax.mapping
import scala.collection.immutable.NumericRange
import scala.util.control.Breaks.*
import zio.stream.ZSink
import zio.stream.ZStream
import zio.ZIO

object Part2 extends ChallengeAsync(day(5)):
  def execute =
    for {
      result <- input.run(ZSink.head.zip(ZSink.collectAll))
      (seed, tail) = (result._1.get, result._2.toList)
      mappigs = Mapping.constructAll(tail)
      seeds = ZStream.fromIterator(
        Seeds.parse(seed).grouped(2).map {
          case head :: next :: Nil => iterator(head, next)
          case x =>
            println(x.toList)
            ???
        },
        5
      )
      min <- seeds
        .mapZIO { seedRange =>
          ZStream
            .from(seedRange)
            .mapZIOPar(5) { s =>
              ZIO.succeedBlocking(Mapping.resolve(Seed, Location, s)(mappigs))
            }
            .runFold(-999L) { (cmin, seed) =>
              if cmin == -999L then seed else cmin min seed
            }
        }
        .runFold(-999L) { (cmin, seed) =>
          if cmin == -999L then seed else cmin min seed
        }
    } yield min

    // // var minlocation: Option[Long] = None
    // flattenned
    //   .scanLeft(-999L) { (cmin, seed) =>
    //     val location = Mapping.resolve(Seed, Location, seed)(mappings)
    //     if cmin == -999L then location else cmin min location
    //   }
    //   .zipWithIndex
    //   .foreach { case (min, i) =>
    //     print(s"\r[$i]: $min")
    //   }

    // 0

def iterator(start: Long, n: Long): Iterator[Long] = new Iterator[Long] {
  var nxt: Option[Long] = Some(start)
  override def hasNext: Boolean = {
    nxt != None
  }

  def next = {
    nxt.fold(throw IndexOutOfBoundsException(s"$nxt: out of range")) { r =>
      if r + 1 == (start + n) then nxt = None else nxt = Some(r + 1)
      r
    }
  }
}
