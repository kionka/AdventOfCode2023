import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser}

object Day2:
  private val number: Parser[Long] = Numbers.digits.mapFilter(_.toLongOption)

  private case class Sample(red: Long, green: Long, blue: Long)

  private sealed trait Color
  private case class Red(amount: Long) extends Color
  private case class Green(amount: Long) extends Color
  private case class Blue(amount: Long) extends Color

  private def colorNameParser(name: String): Parser[Long] = number <* Parser.string(" " + name)
  private val redParser: Parser[Red] = colorNameParser("red").map(Red.apply)
  private val greenParser: Parser[Green] = colorNameParser("green").map(Green.apply)
  private val blueParser: Parser[Blue] = colorNameParser("blue").map(Blue.apply)
  private val colorParser: Parser[Color] = redParser.backtrack | greenParser.backtrack | blueParser

  // this just takes the last number for a color, so it assumes correct input
  private val sampleParser: Parser[Sample] = colorParser.repSep(Parser.string(", ")).map { colors =>
    colors.foldLeft(Sample(0, 0, 0)) { (accumulator, color) =>
      color match
        case Red(amount) => accumulator.copy(red = amount)
        case Green(amount) => accumulator.copy(green = amount)
        case Blue(amount) => accumulator.copy(blue = amount)
    }
  }
  private val samplesParser: Parser[NonEmptyList[Sample]] = sampleParser.repSep(Parser.string("; "))

  private val gameNumberParser: Parser[Long] = Parser.string("Game ") *> number <* Parser.string(": ")

  private case class ParsedLine(gameNumber: Long, samples: NonEmptyList[Sample])

  private val lineParser: Parser[ParsedLine] = (gameNumberParser ~ samplesParser).map { (number, samples) =>
    ParsedLine(number, samples)
  }

  private val maxRed = 12
  private val maxGreen = 13
  private val maxBlue = 14

  private def violatesMaximums(samples: NonEmptyList[Sample]): Boolean = samples.exists { sample =>
    sample.red > maxRed || sample.green > maxGreen || sample.blue > maxBlue
  }

  def puzzle1(lines: List[String]): Long =
    lines.map(lineParser.parseAll).map {
      case Left(error) => throw IllegalArgumentException(error.toString)
      case Right(line) => if violatesMaximums(line.samples) then
        0
      else
        line.gameNumber
    }.sum

  private case class Maximum(red: Long, green: Long, blue: Long)

  private def findMaximum(samples: NonEmptyList[Sample]): Maximum = samples.foldLeft(Maximum(0, 0, 0)) {
    (accumulator, sample) =>
      val red = sample.red.max(accumulator.red)
      val green = sample.green.max(accumulator.green)
      val blue = sample.blue.max(accumulator.blue)
      Maximum(red, green, blue)
  }

  def puzzle2(lines: List[String]): Long =
    lines.map(lineParser.parseAll).map {
      case Left(error) => throw IllegalArgumentException(error.toString)
      case Right(line) =>
        val maximum = findMaximum(line.samples)
        maximum.red * maximum.green * maximum.blue
    }.sum
