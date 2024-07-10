import cats.effect.{IO, Resource}

import scala.io.Source

object Day1Puzzle1:
  def calibrate(): IO[Long] =
    val inputResource: Resource[IO, Source] = Resource.make(open)(close)
    inputResource.use(source => IO.blocking(source.getLines().toVector)).map(lines => lines.map(parseLine).sum)
    
  private val open: IO[Source] = IO.blocking(Source.fromFile("src/main/resources/day1puzzle1.txt"))
  private def close(source: Source): IO[Unit] = IO.blocking(source.close())

  private def parseLine(line: String): Long =
    val numbers = line.filter(_.isDigit).toVector
    val calibrationValue = numbers.head.toString ++ numbers.last.toString
    calibrationValue.toLong