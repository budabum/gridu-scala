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
  override def value(): Double = 88
}

final case class Minus(a: Expr, b: Expr) extends Expr {
  override def value(): Double = 66
}

final case class Times(a: Expr, b: Expr) extends Expr {
  override def value(): Double = 44
}

final case class Divide(a: Expr, b: Expr) extends Expr {
  override def value(): Double = 22
}

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(e => Signal(eval(e.apply(), Map())))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr.value()
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
