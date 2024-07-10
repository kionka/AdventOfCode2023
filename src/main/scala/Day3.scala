import cats.data.NonEmptyList

import scala.annotation.tailrec

object Day3:

  private case class NumberLocation(start: Int, end: Int, number: Int)
  private case class SymbolLocation(index: Int)
  private case class LineLocations(numbers: List[NumberLocation], symbols: List[SymbolLocation])

  def puzzle1(lines: List[String]): Int =
    def sumParts(above: Option[LineLocations], current: LineLocations, below: Option[LineLocations]): Int =
      val symbolsAbove = above.map(_.symbols.map(_.index)).getOrElse(List.empty)
      val symbolsCurrent = current.symbols.map(_.index)
      val symbolsBelow = below.map(_.symbols.map(_.index)).getOrElse(List.empty)
      current.numbers.map { numberLocation =>
        val range = Range.inclusive(numberLocation.start - 1, numberLocation.end + 1)
        if symbolsAbove.exists(range.contains) || symbolsCurrent.exists(range.contains) || symbolsBelow.exists(range.contains) then
          numberLocation.number
        else
          0
      }.sum
    def symbol(char: Char): Boolean = !char.isDigit && char != '.'
    slidingMap(lines.map(parseLine(_, symbol)), sumParts).sum

  def puzzle2(lines: List[String]): Int =
    def gearRatios(above: Option[LineLocations], current: LineLocations, below: Option[LineLocations]): Int =
      val numbersAbove = above.map(_.numbers).getOrElse(List.empty)
      val numbersCurrent = current.numbers
      val numbersBelow = below.map(_.numbers).getOrElse(List.empty)
      current.symbols.map { symbolLocation =>
        val index = symbolLocation.index
        val range = Range.inclusive(index - 1, index + 1)
        def touches(numbers: List[NumberLocation]): List[Int] =
          numbers.filter(number => Range.inclusive(number.start, number.end).exists(range.contains)).map(_.number)
        val touching = touches(numbersAbove) ++ touches(numbersCurrent) ++ touches(numbersBelow)
        if touching.size == 2 then
          touching.product
        else
          0
      }.sum
    def symbol(char: Char): Boolean = char == '*'
    slidingMap(lines.map(parseLine(_, symbol)), gearRatios).sum

  private def slidingMap[A, B](list: List[A], f: (Option[A], A, Option[A]) => B): List[B] =
    @tailrec
    def secondCase(remaining: NonEmptyList[A], previous: A, accumulator: List[B]): List[B] = remaining.tail match
      case second :: next =>
        secondCase(
          NonEmptyList(second, next),
          remaining.head,
          accumulator.appended(f(Some(previous), remaining.head, Some(second)))
        )
      case Nil => accumulator.appended(f(Some(previous), remaining.head, None))

    list match
      case head :: second :: next => secondCase(NonEmptyList(second, next), head, List(f(None, head, Some(second))))
      case head :: Nil => List(f(None, head, None))
      case Nil => List.empty

  @tailrec
  private def groupByChange[A](remaining: List[A], change: (A, A) => Boolean, accumulator: List[NonEmptyList[A]]): List[NonEmptyList[A]] =
    remaining match
      case head :: next => accumulator match
        case grouped :: previousGrouped =>
          if change(grouped.last, head) then
            groupByChange(next, change, NonEmptyList.one(head) :: accumulator)
          else
            groupByChange(next, change, grouped.append(head) :: previousGrouped)
        case Nil => groupByChange(next, change, List(NonEmptyList.one(head)))
      case Nil => accumulator.reverse // reverse is not necessary, but it keeps the groupings in order

  private def parseLine(line: String, symbol: Char => Boolean): LineLocations =
    val indexedLine = line.zipWithIndex
    val symbolLocations = indexedLine.flatMap {
      (char, int) => if symbol(char) then
        Some(SymbolLocation(int))
      else
        None
    }.toList
    val indexedDigits = indexedLine.flatMap {
      (char, int) => if char.isDigit then
        Some((char, int))
      else
        None
    }.toList
    def gapInIndex(digit: (Char, Int), nextDigit: (Char, Int)): Boolean = nextDigit._2 - digit._2 > 1
    val numberLocations = groupByChange(indexedDigits, gapInIndex, List.empty).map { grouping =>
      NumberLocation(grouping.head._2, grouping.last._2, grouping.map(_._1).toList.mkString.toInt)
    }
    LineLocations(numberLocations, symbolLocations)
