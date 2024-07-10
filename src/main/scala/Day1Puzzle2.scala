import cats.effect.{IO, Resource}

import scala.annotation.tailrec
import scala.io.Source

object Day1Puzzle2:
  def calibrate(): IO[Long] =
    val inputResource: Resource[IO, Source] = Resource.make(open)(close)
    inputResource.use(source => IO.blocking(source.getLines().toVector)).map(lines => lines.map(parseLine).sum)

  private val open: IO[Source] = IO.blocking(Source.fromFile("src/main/resources/day1puzzle1.txt"))
  private def close(source: Source): IO[Unit] = IO.blocking(source.close())

  private enum Numbers(val numeral: String, val spelled: String):
    case One extends Numbers("1", "one")
    case Two extends Numbers("2", "two")
    case Three extends Numbers("3", "three")
    case Four extends Numbers("4", "four")
    case Five extends Numbers("5", "five")
    case Six extends Numbers("6", "six")
    case Seven extends Numbers("7", "seven")
    case Eight extends Numbers("8", "eight")
    case Nine extends Numbers("9", "nine")

  private object Numbers:
    def contains(input: String): Option[Numbers] =
      def matches(number: Numbers): Boolean = input.contains(number.numeral) || input.contains(number.spelled)
      @tailrec
      def search(remaining: List[Numbers]): Option[Numbers] =
        remaining match
          case number :: rest =>
            if matches(number) then
              Some(number)
            else
              search(rest)
          case Nil => None
      search(Numbers.values.toList)

  def parseLine(line: String): Long =
    val input = line.toList.map(_.toString)
    @tailrec
    def findFirst(accumulator: String, next: List[String], isForward: Boolean): Numbers =
      next match
        case character :: rest =>
          val newAccumulator = if isForward then
            accumulator ++ character
          else
            character ++ accumulator
          Numbers.contains(newAccumulator) match
            case Some(number) => number
            case None => findFirst(newAccumulator, rest, isForward)
        case Nil => throw IllegalArgumentException(s"This line cannot be parsed: $line")
    val firstNumber = findFirst("", input, true)
    val lastNumber = findFirst("", input.reverse, false)
    val calibrationValue = firstNumber.numeral ++ lastNumber.numeral
    calibrationValue.toLong