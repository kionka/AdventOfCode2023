import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import cats.parse.{Numbers, Parser}

object Day6:

  private val number: Parser[Long] = Numbers.digits.mapFilter(_.toLongOption)
  private def parse[A](parser: Parser[A], string: String): A = parser.parseAll(string) match
    case Left(value) => throw IllegalArgumentException(value.toString)
    case Right(value) => value

  def puzzle1(lines: List[String]): Long =
    val (times, distances) = parseLines(lines)
    val races = times.zip(distances)
    multiplyWinningPossibilities(races)

  def puzzle2(lines: List[String]): Long =
    val (times, distances) = parseLines(lines)
    val time = times.map(_.toString).toList.mkString.toLong
    val distance = distances.map(_.toString).toList.mkString.toLong
    multiplyWinningPossibilities(NonEmptyList.one((time, distance)))

  private def parseLines(lines: List[String]): (NonEmptyList[Long], NonEmptyList[Long]) =
    val timesParser = Parser.string("Time:") *> sp.rep *> (number <* sp.rep.?).rep
    val distancesParser = Parser.string("Distance:") *> sp.rep *> (number <* sp.rep.?).rep
    (parse(timesParser, lines.head), parse(distancesParser, lines.tail.head))

  private def multiplyWinningPossibilities(races: NonEmptyList[(Long, Long)]): Long =
    races.foldLeft(1L) {
      case (accumulator, (time, distance)) =>
        // quadratic equation to get roots for -x^2 + time * x - distance > 0
        // (-b +/- sqrt(b^2 - 4ac)) / 2a
        val largerRoot = ((time + Math.sqrt(Math.pow(time.toDouble, 2.0) - (4 * distance))) / 2).floor.round
        val smallerRoot = ((time - Math.sqrt(Math.pow(time.toDouble, 2.0) - (4 * distance))) / 2).ceil.round
        val positiveIntegers = largerRoot - smallerRoot + 1
        accumulator * positiveIntegers
    }
