package pokerhands

trait Deck {
  type Suit = String
  type Rank = String
  case class Card (rank: Rank, suit: Suit) {
    override def toString: String = rank.toString ++ suit.toString
  }
  def allCards: Seq[Card]
  def straightOrderings: Set[Seq[Rank]]
  def cards (cardNames: String): Set[Card] =
    cardNames.split("\\s").map { s =>
      allCards.find(_.toString == s) match {
        case None => sys.error(s"No card named $s")
        case Some(c) => c
      }
    }.toSet
}

case class Valuator (
    deck: Deck, cardsPerHand: Int, minStraight: Int, minFlush: Int,
    minKind: Int = 2) {
  import deck._
  type Hand = String
  val cardsByRank: Map[Rank,Set[Card]] =
    allCards.groupBy(_.rank).mapValues(_.toSet)
  val straightRanks: Map[Rank,Set[Seq[Rank]]] = {
    val ts: Set[Seq[Rank]] = straightOrderings.flatMap(rs => rs.tails.toList)
    ts.filter(! _.isEmpty).groupBy(_.head)
  }.withDefaultValue(Set.empty[Seq[Rank]])
  def nameStraightOrFlush (s: String) (n: Int, minSize: Int): String =
    if (minSize == cardsPerHand) s
    else if (minSize < cardsPerHand - 1) s"$n-$s"
    else if (n == minSize) s"small $s"
    else s"big $s"
  def nameNOfAKind (n: Int, minSize: Int): String =
    if (n == 2) "pair" else s"$n of a kind"
  def nameHand (
        h: Set[Card], cards: Set[Card], namer: (Int,Int) => String,
        minSize: Int, recheck: Set[Card] => Option[Hand]):
      Option[Hand] = {
    val n = h.size
    if (n < minSize)
      None
    else {
      val nm = namer(n,minSize)
      val remHand = cards -- h
      if (remHand.isEmpty)
        Some(nm)
      else recheck(remHand) match {
        case None => Some(nm)
        case Some(s) => Some(nm + " + " + s)
      }
    }
  }
  def checkStraights (cards: Set[Card]): Option[Hand] = {
    def longestStraight (cs: Set[Card], next: Seq[Rank]): Set[Card] =
      next.headOption match {
        case None =>
          Set.empty
        case Some(r) =>
          cs.find(_.rank == r) match {
            case None => Set.empty
            case Some(c) => longestStraight(cs - c,next.tail) + c
          }
      }
    val maxStraight: Set[Card] =
      cards.foldLeft(Set.empty[Card]) { case (s,c) =>
        straightRanks(c.rank).foldLeft(s) { case (s,rs) =>
          val strt = longestStraight(cards,rs)
          if (strt.size > s.size) strt else s
        }
      }
    nameHand(
      maxStraight,cards,nameStraightOrFlush("straight"),minStraight,
      checkStraights _)
  }
  def checkFlushes (cards: Set[Card]): Option[Hand] = {
    val maxSuit = cards.groupBy(_.suit).toSeq.map(_._2).maxBy(_.size)
    nameHand(maxSuit,cards,nameStraightOrFlush("flush"),minFlush,checkFlushes _)
  }
  def checkKinds (cards: Set[Card]): Option[Hand] = {
    val maxRank = cards.groupBy(_.rank).toSeq.map(_._2).maxBy(_.size)
    nameHand(maxRank,cards,nameNOfAKind _,minKind,checkKinds _)
  }    
  def value (cards: Set[Card]): Hand = {
    val goodHand =
      Seq(checkStraights(cards),checkFlushes(cards),checkKinds(cards)).flatten
    if (goodHand.isEmpty) "high card" else goodHand.mkString(" ")
  }
  def rankHands: Seq[(Hand,Double)] = {
    val handCounts = 
      collection.mutable.Map.empty[Hand,Long].withDefaultValue(0L)
    def checkHands (cards: Set[Card], from: Seq[Card], remaining: Int): Unit =
      if (remaining == 0) {
        val h = value(cards)
        val newCount = handCounts(h) + 1L
        if (java.lang.Long.bitCount(newCount) == 1)
          println(
            s"Seen $h $newCount times at ${cards.mkString("<<"," ",">>")}")
        handCounts += (h -> newCount)
      } else {
        val rem = remaining - 1
        from.tails.foreach { cs =>
          if (cs.size >= remaining)
            checkHands(cards + cs.head,cs.tail,rem)
        }
      }
    checkHands(Set.empty,allCards,cardsPerHand)
    val handsCounted = handCounts.map(_._2).sum
    val maxHandLen = handCounts.map(_._1).maxBy(_.length).length
    println(
      s"Counted $handsCounted hands with ${handCounts.size} different values")
    def show [A] (m: Seq[(Hand,A)], f: A => A, fmt: String): Unit = {
      println
      println(
        m.map { case (h,a) => s"%${maxHandLen}s  $fmt".format(h,f(a)) }.
          mkString("\n"))
      println
    }
    show(handCounts.toSeq.sortBy(_._2),(n: Long) => n,"%d")
    val result =
      handCounts.toSeq.map { case (h,n) => (h,n.toDouble / handsCounted) }.
        sortBy(_._2)
    show(result,(d: Double) => d * 100.0,"%2.6f%%")
    result
  }
}

object StandardDeck extends Deck {
  val allCards =
    Seq("A","2","3","4","5","6","7","8","9","10","J","Q","K").flatMap { r =>
      Seq("S","H","D","C").map(s => Card(r,s))
    }
  val straightOrderings =
    Set(Seq("A","2","3","4","5","6","7","8","9","10","J","Q","K","A"))
}

object TarotDeck extends Deck {
  val allCards =
    Seq("1","2","3","4","5","6","7","8","9","10","V","C","D","R").flatMap { r =>
      Seq("S","H","D","C").map(s => Card(r,s))
    } ++
      (0 to 21).map(r => Card(r.toString,"T"))
  val straightOrderings =
    Set(
      Seq("1","2","3","4","5","6","7","8","9","10","V","C","D","R","1"),
      ((0 to 21) :+ 0).map(_.toString))
}

object FanucciDeck extends Deck {
  val allCards =
    Seq("0","1","2","3","4","5","6","7","8","9","n").flatMap { r =>
      Seq(
          "Mz","Bk","Rn","Bg","Fr","In","Sc","Pl","Fc","Tm","Lm","Hv","Er",
          "Zr","Tp").
        map(s => Card(r,s)) 
    } ++
      Seq("Grn","Dth","Lgt","Snl","Bty","Tim","Gru","Lob","Jst").map { r =>
        Card(r,"")
      }
  val straightOrderings =
    Set(Seq("0","1","2","3","4","5","6","7","8","9"))
}
