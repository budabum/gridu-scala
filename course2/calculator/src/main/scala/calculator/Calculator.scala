package calculator

sealed abstract class Expr {
  def value(): Double
}

final case class Literal(v: Double) extends Expr {
  override def value(): Double = v
}

final case class Ref(name: String) extends Expr {
  override def value(): Double = 77
}

final case class Plus(a: Expr, b: Expr) extends Expr {
  override def value(): Double = a.value + b.value
}

final case class Minus(a: Expr, b: Expr) extends Expr {
  override def value(): Double = a.value - b.value
}

final case class Times(a: Expr, b: Expr) extends Expr {
  override def value(): Double = a.value * b.value
}

final case class Divide(a: Expr, b: Expr) extends Expr {
  override def value(): Double = a.value / b.value
}

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(e => Signal(eval(e.apply(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(x) => x
      case Ref(v) => eval(getReferenceExpr(v, references), references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
      case _ => 888
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
