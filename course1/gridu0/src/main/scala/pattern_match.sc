trait Expr  {
  override def toString: String = {
    show(this)
  }
}
case class Num(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(v: String) extends Expr

object Num {
  def apply(n: Int) = new Num(n)
}

object Sum {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}

object Prod {
  def apply(e1: Expr, e2: Expr) = new Prod(e1, e2)
}

object Var {
  def apply(v: String) = new Var(v)
}

def eval(e: Expr): Int = e match {
  case Num(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Prod(e1, e2) => eval(e1) * eval(e2)
//  case Var(v) => v
}

def show(e: Expr): String = {
  def s(e: Expr, b: Boolean): String = {
    e match {
      case Num(n) => n.toString
      case Sum(e1, e2) => {
        if (b) "(" + s(e1, b) +" + "+ s(e2, b) + ")"
        else s(e1, b) +" + "+ s(e2, b)
      }
      case Prod(e1, e2) => s(e1, true) +" * "+ s(e2, true)
      case Var(v) => v
    }
  }

  s(e, false)
}

show(Num(99))
show(Sum(Num(12), Num(21)))
show(Prod(Num(3), Num(4)))
show(Sum(Num(11), Sum(Num(12), Num(21))))
show(Var("x"))
show(Sum(Num(1), Var("x")))
show(Sum(Prod(Num(2), Var("x")), Var("y")))
show(Prod(Sum(Num(2), Var("x")), Var("y")))

println(":" + Num(21))
println("::"+ Sum(Num(12), Num(21)))

