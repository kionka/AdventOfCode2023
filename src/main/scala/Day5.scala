import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import cats.parse.{Numbers, Parser}

import scala.annotation.tailrec

object Day5:

  private val number: Parser[Long] = Numbers.digits.mapFilter(_.toLongOption)
  private val newLine: Parser[Unit] = Parser.string("\n")
  private def parse[A](parser: Parser[A], string: String): (String, A) = parser.parse(string) match
    case Left(value) => throw IllegalArgumentException(value.toString)
    case Right(value) => value

  private case class Offset(destinationCategory: Long, sourceCategory: Long, rangeLength: Long):
    def destination(source: Long): Option[Long] =
      val sourceRange = sourceCategory + rangeLength - 1
      if source >= sourceCategory && source <= sourceRange then
        Some(source - sourceCategory + destinationCategory)
      else
        None

  private case class Offsets(offsets: List[Offset]):
    def destination(source: Long): Long = offsets.flatMap(_.destination(source)).headOption.getOrElse(source)

  private def seedsAndLocation(lines: List[String]): (NonEmptyList[Long], Long => Long) =
    val input = lines.mkString("", "\n", "\n")
    val seedsParser = Parser.string("seeds: ") *> (number <* sp.?).rep <* Parser.string("\n\n")
    // TODO: define ~~ or something
    val offset = ((number <* sp) ~ (number <* sp) ~ (number <* newLine)).map {
      case ((destination, source), range) => Offset(destination, source, range)
    }
    val offsets = offset.rep.map(nel => Offsets(nel.toList)) <* newLine

    val seedToSoilParser = Parser.string("seed-to-soil map:\n") *> offsets
    val soilToFertilizerParser = Parser.string("soil-to-fertilizer map:\n") *> offsets
    val fertilizerToWaterParser = Parser.string("fertilizer-to-water map:\n") *> offsets
    val waterToLightParser = Parser.string("water-to-light map:\n") *> offsets
    val lightToTemperatureParser = Parser.string("light-to-temperature map:\n") *> offsets
    val temperatureToHumidityParser = Parser.string("temperature-to-humidity map:\n") *> offsets
    val humidityToLocationParser =
      Parser.string("humidity-to-location map:\n") *> offset.rep.map(nel => Offsets(nel.toList)) <* Parser.end

    val (sts, seeds) = parse(seedsParser, input)
    val (stf, seedToSoil) = parse(seedToSoilParser, sts)
    val (ftw, soilToFertilizer) = parse(soilToFertilizerParser, stf)
    val (wtl, fertilizerToWater) = parse(fertilizerToWaterParser, ftw)
    val (ltt, waterToLight) = parse(waterToLightParser, wtl)
    val (tth, lightToTemperature) = parse(lightToTemperatureParser, ltt)
    val (htl, temperatureToHumidity) = parse(temperatureToHumidityParser, tth)
    val (_, humidityToLocation) = parse(humidityToLocationParser, htl)

    val location =
      seedToSoil.destination
        .andThen(soilToFertilizer.destination)
        .andThen(fertilizerToWater.destination)
        .andThen(waterToLight.destination)
        .andThen(lightToTemperature.destination)
        .andThen(temperatureToHumidity.destination)
        .andThen(humidityToLocation.destination)

    (seeds, location)

  def puzzle1(lines: List[String]): Long =
    val (seeds, location) = seedsAndLocation(lines)
    seeds.map(location).toList.min

  // this can be made fast by collapsing the seed to location ranges and then partitioning the seed ranges based on that
  // and taking the lowest value in each seed range and computing the location because they are linear
  def puzzle2(lines: List[String]): Long =
    val seedsParser = Parser.string("seeds: ") *> ((number <* sp) ~ (number <* sp.?)).rep
    val location = seedsAndLocation(lines)._2
    val seeds = parse(seedsParser, lines.head)._2
    @tailrec
    def minimum(currentSeed: Long, seedMax: Long, currentMinimum: Long): Long =
      val min = Math.min(currentMinimum, location(currentSeed))
      if currentSeed == seedMax then
        min
      else
        minimum(currentSeed + 1, seedMax, min)
    seeds.foldLeft(location(seeds.head._1)) {
      case (currentMinimum, (seedStart, seedRange)) => minimum(seedStart, seedStart + seedRange - 1, currentMinimum)
    }
