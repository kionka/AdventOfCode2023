import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import cats.parse.{Numbers, Parser}

object Day7:

  private val number: Parser[Long] = Numbers.digits.mapFilter(_.toLongOption)
  private def parse[A](parser: Parser[A], string: String): A = parser.parseAll(string) match
    case Left(value) => throw IllegalArgumentException(value.toString)
    case Right(value) => value

  private enum Card(val character: Char) extends Ordered[Card]:
    case Two extends Card('2')
    case Three extends Card('3')
    case Four extends Card('4')
    case Five extends Card('5')
    case Six extends Card('6')
    case Seven extends Card('7')
    case Eight extends Card('8')
    case Nine extends Card('9')
    case Ten extends Card('T')
    case Jack extends Card('J')
    case Queen extends Card('Q')
    case King extends Card('K')
    case Ace extends Card('A')
    // TODO: remove this and replace with Ordering (wilds)
    override def compare(that: Card): Int = this.ordinal - that.ordinal

  private object Card:
    def apply(character: Char): Option[Card] = values.find(_.character == character)

  private enum HandType extends Ordered[HandType]:
    private case HighCard, OnePair, TwoPair, ThreeKind, FullHouse, FourKind, FiveKind
    override def compare(that: HandType): Int = this.ordinal - that.ordinal

  private object HandType:
    def applyNoWilds(cards: NonEmptyList[Card]): HandType =
      val pairings = duplicates(cards.toList)
      if pairings == List(5) then
        FiveKind
      else if pairings == List(4) then
        FourKind
      else if pairings.contains(3) && pairings.contains(2) then
        FullHouse
      else if pairings.contains(3) && !pairings.contains(2) then
        ThreeKind
      else if pairings == List(2, 2) then
        TwoPair
      else if pairings == List(2) then
        OnePair
      else
        HighCard

    def applyWilds(cards: NonEmptyList[Card]): HandType =
      val (normals, jacks) = cards.toList.partition(_ != Card.Jack)
      val wilds = jacks.size
      val pairings = duplicates(normals)
      if pairings == List(5) then
        FiveKind
      else if pairings == List(4) then
        if wilds == 1 then
          FiveKind
        else
          FourKind
      else if pairings.contains(3) && pairings.contains(2) then
        FullHouse
      else if pairings.contains(3) && !pairings.contains(2) then
        if wilds == 2 then
          FiveKind
        else if wilds == 1 then
          FourKind
        else
          ThreeKind
      else if pairings == List(2, 2) then
        if wilds == 1 then
          FullHouse
        else
          TwoPair
      else if pairings == List(2) then
        if wilds == 3 then
          FiveKind
        else if wilds == 2 then
          FourKind
        else if wilds == 1 then
          ThreeKind
        else
          OnePair
      else
        if wilds == 5 || wilds == 4 then
          FiveKind
        else if wilds == 3 then
          FourKind
        else if wilds == 2 then
          ThreeKind
        else if wilds == 1 then
          OnePair
        else
          HighCard

  // finds how many groups of cards are the same to assist in finding the hand type
  private def duplicates[A](list: List[A]): List[Int] =
    list.foldLeft(Map.empty[A, Int]) {
      (seen, element) =>
        seen.updatedWith(element) {
          case Some(value) => Some(value + 1)
          case None => Some(1)
        }
    }.values.toList.filter(_ > 1)

  private case class Hand(handType: HandType, hand: NonEmptyList[Card], bet: Long) extends Ordered[Hand]:
    // TODO: remove this and replace with Ordering (wilds)
    override def compare(that: Hand): Int =
      val handTypeResult = this.handType.compare(that.handType)
      if handTypeResult != 0 then
        handTypeResult
      else
        val cardResults = this.hand.zip(that.hand).map((first, second) => first.compare(second)).filter(_ != 0)
        cardResults match
          case head :: _ => head
          case Nil => 0

  private object Hand:
    def createNoWilds(hand: NonEmptyList[Card], bet: Long): Hand = Hand(HandType.applyNoWilds(hand), hand, bet)
    def createWilds(hand: NonEmptyList[Card], bet: Long): Hand = Hand(HandType.applyWilds(hand), hand, bet)

  def puzzle1(lines: List[String]): Long =
    val handSize = 5
    val cardsParser: Parser[NonEmptyList[Card]] =
      Parser.charIn(Card.values.map(_.character)).mapFilter(Card.apply).repExactlyAs(handSize)
    val betParser: Parser[Long] = sp *> number
    val lineParser = cardsParser ~ betParser
    val parsedLines = lines.map(parse(lineParser, _))
    val hands = parsedLines.map(Hand.createNoWilds.tupled).sorted
    val winnings = hands.map(_.bet).zipWithIndex.map((bet, index) => bet * (index + 1))
    winnings.sum
