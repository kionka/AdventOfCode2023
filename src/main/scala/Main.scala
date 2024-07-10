import cats.Foldable
import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO, IOApp}

import scala.io.Source

object Main extends IOApp:

  private val day2File = "src/main/resources/day2.txt"
  private val day3File = "src/main/resources/day3.txt"
  private val day4File = "src/main/resources/day4.txt"
  private val day5File = "src/main/resources/day5.txt"
  private val day6File = "src/main/resources/day6.txt"
  private val day7File = "src/main/resources/day7.txt"
  private def close(source: Source): IO[Unit] = IO.blocking(source.close())
  private def processLines(filename: String, solution: List[String] => Long): IO[Long] =
    val open: IO[Source] = IO.blocking(Source.fromFile(filename))
    val inputResource: Resource[IO, Source] = Resource.make(open)(close)
    inputResource.use(source => IO.blocking(source.getLines().toList)).map(solution)

  // Day 1 Puzzle 1: 54940
  // Day 1 Puzzle 2: 54208
  // Day 2 Puzzle 1: 2207
  // Day 2 Puzzle 2: 62241
  // Day 3 Puzzle 1: 529618
  // Day 3 Puzzle 2: 77509019
  // Day 4 Puzzle 1: 21213
  // Day 4 Puzzle 2: 8549735
  // Day 5 Puzzle 1: 174137457
  // Day 5 Puzzle 2: 1493866
  // Day 6 Puzzle 1: 4568778
  // Day 6 Puzzle 2: 28973936
  // Day 7 Puzzle 1: 251287184
  override def run(args: List[String]): IO[ExitCode] =
    for
      result11 <- Day1Puzzle1.calibrate()
      result12 <- Day1Puzzle2.calibrate()
      result21 <- processLines(day2File, Day2.puzzle1)
      result22 <- processLines(day2File, Day2.puzzle2)
      result31 <- processLines(day3File, Day3.puzzle1)
      result32 <- processLines(day3File, Day3.puzzle2)
      result41 <- processLines(day4File, Day4.puzzle1)
      result42 <- processLines(day4File, Day4.puzzle2)
      result51 <- processLines(day5File, Day5.puzzle1)
      // result52 <- processLines(day5File, Day5.puzzle2) // TODO: slow
      result61 <- processLines(day6File, Day6.puzzle1)
      result62 <- processLines(day6File, Day6.puzzle2)
      result71 <- processLines(day7File, Day7.puzzle1)
      _ <- IO.println(s"Day 1 Puzzle 1: $result11")
      _ <- IO.println(s"Day 1 Puzzle 2: $result12")
      _ <- IO.println(s"Day 2 Puzzle 1: $result21")
      _ <- IO.println(s"Day 2 Puzzle 2: $result22")
      _ <- IO.println(s"Day 3 Puzzle 1: $result31")
      _ <- IO.println(s"Day 3 Puzzle 2: $result32")
      _ <- IO.println(s"Day 4 Puzzle 1: $result41")
      _ <- IO.println(s"Day 4 Puzzle 2: $result42")
      _ <- IO.println(s"Day 5 Puzzle 1: $result51")
      // _ <- IO.println(s"Day 5 Puzzle 2: $result52")
      _ <- IO.println(s"Day 6 Puzzle 1: $result61")
      _ <- IO.println(s"Day 6 Puzzle 2: $result62")
      _ <- IO.println(s"Day 7 Puzzle 1: $result71")
    yield
      ExitCode.Success

  def foldLeftWhileRight[Input, Accumulator](
    list: List[Input],
    accumulator: Accumulator,
    function: (Accumulator, Input) => Either[Accumulator, Accumulator]
  ): Accumulator = Foldable[List].foldM(list, accumulator)(function).merge
