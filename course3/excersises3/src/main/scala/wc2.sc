def tr1(x: Int): Tr1[Int]

trait Tr1[T]{
  def fff(x: T): T = {
    x * x
  }
}

