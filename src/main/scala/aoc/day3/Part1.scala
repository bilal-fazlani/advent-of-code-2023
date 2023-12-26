package aoc
package day3

import zio.*

object Part1 extends Challenge[Int](day(3)):
  case class DigitScan(int: Char, isAdjacentToSymbol: Boolean)

  def execute =
    for {
      allLines <- file.runCollect
      metrix = allLines.map(readLine)
      length = metrix.length
      width = metrix.head.length
      scannedDigits =
        metrix.zipWithIndex
          .map(x => (x._1.zipWithIndex, x._2))
          .map(x =>
            x._1.map { y =>
              scanDigit(metrix, x._2, y._2, width, length)
            }
          )
      numbersScanned = findNumbers(scannedDigits)
      sum = numbersScanned.collect { case Number(value, true) =>
        value
      }.sum
    } yield sum

  def readLine(line: String) = Chunk.from(line.map(Cell.parse))

  private def neighbours(x: Int, y: Int, metrix: Chunk[Chunk[Cell]]) =
    val product = Chunk
      .from(
        for {
          xs <- (x - 1) to (x + 1)
          ys <- (y - 1) to (y + 1)
          if !(xs == x && ys == y)
        } yield (xs, ys)
      )
      .filter((x1, y1) => x1 >= 0 && y1 >= 0 && x1 < metrix.head.length && y1 < metrix.length)
      .map((x, y) => metrix(x)(y))
    product

  def scanDigit(
      metrix: Chunk[Chunk[Cell]],
      x: Int,
      y: Int,
      width: Int,
      height: Int
  ): Option[DigitScan] =
    val cell = metrix(x)(y)
    cell.digit.map { int =>
      val ns = neighbours(x, y, metrix)
      val isAdjacent = ns.exists(x => x.isSpecial || x.isStar)
      if isAdjacent then DigitScan(int, true)
      else DigitScan(int, false)
    }

  case class Number(value: Int, isPartNumber: Boolean)

  def findNumbers(scan: Chunk[Chunk[Option[DigitScan]]]): Chunk[Number] =
    var currentGroup: Chunk[DigitScan] = Chunk.empty
    var currentPart: Boolean = false

    var done: Chunk[Number] = Chunk.empty

    def isInProgress = currentGroup.nonEmpty

    scan.foreach(row =>
      row.foreach {
        case Some(digit) =>
          currentGroup = currentGroup.appended(digit)
          if digit.isAdjacentToSymbol then currentPart = true
        case None =>
          // if some number is in progress
          if currentGroup.nonEmpty then
            // move it to done
            done = done.appended(Number(currentGroup.map(_.int).mkString.toInt, currentPart))
            // reset current number
            currentGroup = Chunk.empty

          // always part to false when encountering a wall
          currentPart = false
      }
    )
    done
