object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, qs: List[Int] ): Boolean = {
      val row = qs.size
      val board = (row-1 to 0 by -1) zip qs
      board forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }

    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        for {
          qns <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, qns)
        } yield col :: qns
      }
    }

    placeQueens(n)
  }
}

object poly {
  class Poly(termsM: Map[Int, Double]){
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = termsM withDefaultValue 0.0

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    def ++(other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def addTerm(terms: Map[Int, Double], t: (Int, Double)): Map[Int, Double] = {
//      val newV: (Int, Double) = (t._1, t._2 + terms(t._1))
//      terms + newV
      terms + (t._1 -> (t._2 + terms(t._1)))
    }

    def +(other: Poly) = new Poly( (other.terms foldLeft this.terms)(addTerm) )

    override def toString: String = {
      (for( (exp, coeff) <- terms.toList.sorted.reverse )
        yield s"${coeff}x^${exp}") mkString " + "
    }
  }
}

import scala.io.Source

object phonephrase {
  val url = "/home/leshal/work/gridu/scala/forcomp/src/main/resources/forcomp/linuxwords.txt"
  val in = Source.fromFile(url)
  val words = in.getLines().toList filter (w => w.forall(chr => chr.isLetter))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  val charCode: Map[Char, Char] =
    for((digit, str) <- mnem; letter <- str) yield letter -> digit

  def wordCode(word: String): String = {
    word.toUpperCase map charCode
  }

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = {
    if(number.isEmpty) Set(List())
    else{
      for{
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
  }
}

nqueens.queens(4)
import poly._
val p1 = new Poly(1-> 2.0, 3-> 4.0, 5-> 6.2)
val p2 = new Poly(0-> 3.0, 3-> 7.0)
p1.terms(5)
p1 ++ p2
p1 + p2
p2 + p1

println(phonephrase.encode("7225247386"))
