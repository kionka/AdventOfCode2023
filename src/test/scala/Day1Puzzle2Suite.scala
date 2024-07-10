class Day1Puzzle2Suite extends munit.FunSuite:
  test("parses one") {
    val obtained = Day1Puzzle2.parseLine("11")
    val expected = 11L
    assertEquals(obtained, expected)
  }

  test("parses one spelled") {
    val obtained = Day1Puzzle2.parseLine("1one")
    val expected = 11L
    assertEquals(obtained, expected)
  }

  test("parses two spelled") {
    val obtained = Day1Puzzle2.parseLine("1onetwo")
    val expected = 12L
    assertEquals(obtained, expected)
  }

  test("parses three spelled") {
    val obtained = Day1Puzzle2.parseLine("three1onetwo")
    val expected = 32L
    assertEquals(obtained, expected)
  }

  test("parses 15nine1") {
    val obtained = Day1Puzzle2.parseLine("15nine1")
    val expected = 11L
    assertEquals(obtained, expected)
  }

  test("parses rbrftcblxcknine4eight") {
    val obtained = Day1Puzzle2.parseLine("rbrftcblxcknine4eight")
    val expected = 98L
    assertEquals(obtained, expected)
  }

  test("checks for overlapping spelled numbers") {
    val obtained = Day1Puzzle2.parseLine("ftgbfqrzslqrcmmeightnjjrrkvhntcv1djmbqztrkvlqfkshoneightggd")
    val expected = 88L
    assertEquals(obtained, expected)
  }