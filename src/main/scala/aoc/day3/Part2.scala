package aoc
package day3

object Part2 extends ChallengeSync(day(3)):
  case class Position(x: Int, y: Int) {
    override def toString(): String = s"($x, $y)"
  }

  case class HorizontalRange(y: Int, x1: Int, x2: Int) {
    override def toString(): String = s"($x1-$x2, $y)"
  }

  private def intersects(range: HorizontalRange, position: Position): Boolean =
    range.y == position.y && position.x >= range.x1 && position.x <= range.x2

  def execute: Int =
    val metrix = input.map(readLine)
    given List[List[Cell]] = metrix
    val scannedStars = scanStars
    val scannedNumbers = scanNumbers
    scanGearRatios(scannedStars, scannedNumbers).collect { case (_, set) =>
      set.foldLeft[Int](1)(_ * _.value)
    }.sum

  def readLine(line: String) = line.map(Cell.parse).toList

  case class Star(position: Position) {
    override def toString(): String = s"* $position"
  }

  case class PositionedCell(cell: Cell, position: Position) {
    override def toString(): String = s"$cell $position"
  }

  def scanGearRatios(stars: List[Star], numbers: List[Number])(using metrix: List[List[Cell]]) =
    stars
      .map { star =>
        val neighbours = star.position.neighbours
        star -> neighbours
          .flatMap(nb => numbers.filter(num => intersects(num.range, nb.position)))
          .toSet
      }
      .filter(_._2.size > 1)
      .toMap

  def scanStars(using metrix: List[List[Cell]]) =
    metrix.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect { case (Cell.StarSymbol, x) =>
        Star(Position(x, y))
      }
    }

  def scanNumbers(using metrix: List[List[Cell]]) =
    val widht = metrix.head.length
    var currentGroup: List[PositionedCell] = List.empty
    var done: List[Number] = List.empty

    def reset(y: Int) =
      // if some number was in progress
      if currentGroup.nonEmpty then
        // move it to done
        done = done.appended(
          Number(
            currentGroup.map(_.cell.digit.get).mkString.toInt,
            HorizontalRange(y, currentGroup.head.position.x, currentGroup.last.position.x)
          )
        )
        // reset current number
        currentGroup = List.empty

    metrix.zipWithIndex.foreach((row, y) =>
      row.zipWithIndex.foreach {
        case (d @ Cell.Digit(digit), x) if x == widht - 1 =>
          reset(y)
        case (d @ Cell.Digit(digit), x) =>
          currentGroup = currentGroup.appended(PositionedCell(d, Position(x, y)))
        case (_, _) => reset(y)
      }
    )
    done

  extension (position: Position) {
    def neighbours(using metrix: List[List[Cell]]) =
      (for {
        x <- (position.x - 1) to (position.x + 1)
        y <- (position.y - 1) to (position.y + 1)
        if !(x == position.x && y == position.y)
        if x >= 0 && y >= 0 && x < metrix.head.length && y < metrix.length
      } yield (x, y))
        .map { case (x, y) =>
          PositionedCell(metrix(x)(y), Position(x, y))
        }
  }

  case class Number(value: Int, range: HorizontalRange) {
    override def toString(): String = s"[$value] at $range"
  }
