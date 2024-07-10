import cats.parse.Rfc5234.sp
import cats.parse.{Numbers, Parser}

object Day4:

  private val number: Parser[Long] = Numbers.digits.mapFilter(_.toLongOption)

  def puzzle1(lines: List[String]): Int =
    matchesList(lines).map { matches =>
      if matches == 0 then
        0
      else
        math.pow(2, matches - 1).intValue
    }.sum

  def puzzle2(lines: List[String]): Int =
    matchesList(lines).zipWithIndex.foldLeft(Map.empty[Int, Int]) {
      case (copies, (matches, index)) =>
        val totalLineMatches = copies.getOrElse(index, 0) + 1
        // update the current index
        val updatedIndex = copies.updatedWith(index) {
          case Some(indexCopies) => Some(indexCopies + 1)
          case None => Some(1)
        }
        // update upcoming indexes with the current scorecard, this works because everything else should be included
        Range.inclusive(index + 1, index + matches).foldLeft(updatedIndex) {
          (updatingCopies, updatingIndex) => updatingCopies.updatedWith(updatingIndex) {
            case Some(upcomingCopy) => Some(upcomingCopy + totalLineMatches)
            case None => Some(totalLineMatches)
          }
        }
    }.values.sum

  private def matchesList(lines: List[String]): List[Int] =
    val cardHeader = Parser.string("Card") ~ sp.rep ~ number ~ Parser.string(":") ~ sp.rep
    val winningNumbersParser = (number <* sp.rep).rep <* Parser.string("|") <* sp.rep
    val gardnerNumbersParser = (number <* sp.rep0).rep
    val lineParser = cardHeader *> winningNumbersParser ~ gardnerNumbersParser
    lines.map(lineParser.parseAll).map {
      case Left(error) => throw IllegalArgumentException(error.toString)
      case Right((winning, gardner)) => winning.toList.count(gardner.toList.contains)
    }
