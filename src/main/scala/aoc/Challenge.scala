package aoc

import zio.stream.ZStream
import zio.ZIO
import zio.stream.ZPipeline
import zio.ZIOAppDefault
import java.io.IOException

private def read(inputFile: InputFile): ZStream[Any, IOException, String] =
  (inputFile match
    case InputFile.Day(value) =>
      ZStream
        .fromResource(s"day${value}.txt")

    case InputFile.Part(day, part) =>
      val path = s"day${day.value}/part${part}.txt"
      ZStream
        .whenCaseZIO(resourceFileExists(path)) {
          case true  => ZStream.fromResource(path)
          case false => ZStream.fromResource(s"day${day.value}.txt")
        }
  )
  .via(ZPipeline.utfDecode)
    .via(ZPipeline.splitLines)
    .filter(_.nonEmpty)

private def resourceFileExists(path: String) =
  ZIO
    .attemptBlockingIO(
      this.getClass.getClassLoader.getResource(path.replace('\\', '/'))
    )
    .flatMap { url =>
      if (url == null) ZIO.succeed(false)
      else ZIO.succeed(true)
    }

trait Challenge[+A](val input: InputFile) extends ZIOAppDefault:
  val file = read(input)
  def execute: ZIO[Any, Throwable, A]
  def run =
    execute
      .flatMap(value => zio.Console.printLine(value.toString))
      .tapError(error => zio.Console.printLineError(error.getMessage))
      .exitCode

enum InputFile:
  case Day(value: Int)
  case Part(day: Day, value: Int)

def day(number: Int): InputFile.Day = InputFile.Day(number)

extension (day: InputFile.Day)
  def part(number: Int): InputFile.Part = InputFile.Part(day, number)
